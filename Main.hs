{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Lens
import Control.Monad (unless, void)
import Control.Monad.IO.Unlift
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
  , projectInFolder :: FilePath -> a -> m Bool
  , cloneProject    :: FilePath -> a -> m ()
  , pullProject     :: FilePath -> a -> m ()
  }

updateProjects
  :: MonadUnliftIO m => Comp -> FilePath -> Downloader m a -> String -> m ()
updateProjects comp cwd Downloader{..} username =
  getProjects username >>= traverseConcurrently_ comp handleProject
 where
  handleProject p = projectInFolder cwd p >>= \case
    True  -> pullProject cwd p
    False -> cloneProject cwd p >> pullProject cwd p

gitlabDownloader :: Downloader IO Value
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
    <$> get (baseUrl ++ "/users/" ++ username ++ "/projects")

  projectInFolder' cwd value =
    maybe (pure False) (folderExists cwd) (value ^? key "path" . _String)
  cloneProject' cwd value = maybe (pure ()) (cloneGitProject cwd)
    (value ^? key "http_url_to_repo" . _String)
  pullProject' cwd value =
    maybe (pure ()) (pullGitProject cwd) (value ^? key "path" . _String)

folderExists :: FilePath -> Text -> IO Bool
folderExists cwd folder = doesPathExist $ cwd </> T.unpack folder

cloneGitProject :: FilePath -> Text -> IO ()
cloneGitProject cwd url = do
  (_, _, _, p) <- createProcess
    (shell $ "git clone " ++ T.unpack url) { cwd = Just cwd }
  void $ waitForProcess p

pullGitProject :: FilePath -> Text -> IO ()
pullGitProject cwd folder = do
  (_, _, _, p1) <- createProcess (shell "git fetch --all") { cwd = Just path }
  void $ waitForProcess p1
  (_, _, _, p2) <- createProcess (shell "git pull --all") { cwd = Just path }
  void $ waitForProcess p2
 where path = cwd </> T.unpack folder

createFolderIfNotExists :: FilePath -> String -> IO FilePath
createFolderIfNotExists path ext = do
  doesPathExist path' >>= flip unless (createDirectory path')
  return path'
 where path' = path </> ext

main :: IO ()
main = do
  homeDir <- getCurrentDirectory
  gitlabPath <- createFolderIfNotExists homeDir "gitlab"
  updateProjects (ParN 10) gitlabPath gitlabDownloader username
 where username = "DarinM223"

--resp <- get "http://gitlab.com/api/v4/users/DarinM223/projects"
--print $ head (resp ^.. responseBody . values) ^? key "path" . _String
-- print $ gitlabRepos resp

gitlabRepos :: AsValue body => Response body -> [Text]
gitlabRepos = mapMaybe (^? key "http_url_to_repo" . _String)
            . (^.. responseBody . values)
