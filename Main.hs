{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Scheduler
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Network.Wreq
import System.FilePath
import System.Directory
import System.Process
import qualified Data.Text as T

data Downloader m a = Downloader
  { getProjects     :: String -> m [a]
  , projectInFolder :: a -> m Bool
  , cloneProject    :: a -> m ()
  , pullProject     :: a -> m ()
  }

updateProjects :: MonadUnliftIO m => Downloader m a -> Comp -> String -> m ()
updateProjects Downloader{..} comp username =
  getProjects username >>= traverseConcurrently_ comp handleProject
 where
  handleProject p = projectInFolder p >>= \case
    True  -> pullProject p
    False -> cloneProject p >> pullProject p

gitlabDownloader :: Downloader (ReaderT FilePath IO) Value
gitlabDownloader = Downloader
  { getProjects     = getProjects'
  , projectInFolder = projectInFolder'
  , cloneProject    = cloneProject'
  , pullProject     = pullProject'
  }
 where
  baseUrl = "http://gitlab.com/api/v4"

  -- TODO(DarinM223): add authentication
  getProjects' username
    =   (^.. responseBody . values)
    <$> liftIO (get (baseUrl ++ "/users/" ++ username ++ "/projects"))

  projectInFolder' value =
    maybe (pure False) folderExists (value ^? key "path" . _String)
  cloneProject' value =
    maybe (pure ()) cloneGitProject (value ^? key "http_url_to_repo" . _String)
  pullProject' value =
    maybe (pure ()) pullGitProject (value ^? key "path" . _String)

folderExists :: Text -> ReaderT FilePath IO Bool
folderExists folder = ask >>= liftIO . doesPathExist . (</> T.unpack folder)

cloneGitProject :: Text -> ReaderT FilePath IO ()
cloneGitProject url = ask >>= \cwd -> liftIO $ do
  (_, _, _, p) <- createProcess
    (shell $ "git clone " ++ T.unpack url) { cwd = Just cwd }
  void $ waitForProcess p

pullGitProject :: Text -> ReaderT FilePath IO ()
pullGitProject folder = asks (</> T.unpack folder) >>= \path -> liftIO $ do
  (_, _, _, p1) <- createProcess (shell "git fetch --all") { cwd = Just path }
  void $ waitForProcess p1
  (_, _, _, p2) <- createProcess (shell "git pull --all") { cwd = Just path }
  void $ waitForProcess p2

createFolderIfNotExists :: FilePath -> String -> IO FilePath
createFolderIfNotExists path ext = do
  doesPathExist path' >>= flip unless (createDirectory path')
  return path'
 where path' = path </> ext

main :: IO ()
main = do
  homeDir <- getCurrentDirectory
  gitlabPath <- createFolderIfNotExists homeDir "gitlab"
  runReaderT (updateProjects gitlabDownloader (ParN 10) username) gitlabPath
 where username = "DarinM223"

--resp <- get "http://gitlab.com/api/v4/users/DarinM223/projects"
--print $ head (resp ^.. responseBody . values) ^? key "path" . _String
-- print $ gitlabRepos resp

gitlabRepos :: AsValue body => Response body -> [Text]
gitlabRepos = mapMaybe (^? key "http_url_to_repo" . _String)
            . (^.. responseBody . values)
