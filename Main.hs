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
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.Wreq
import System.FilePath
import System.Directory
import System.Process
import qualified Data.ByteString.Char8 as BS
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

gitlabDownloader :: Maybe String -> Downloader (ReaderT FilePath IO) Value
gitlabDownloader accessToken = Downloader
  { getProjects     = getProjects'
  , projectInFolder = projectInFolder'
  , cloneProject    = cloneProject'
  , pullProject     = pullProject'
  }
 where
  baseUrl = "http://gitlab.com/api/v4"
  projectUrl username = baseUrl ++ "/users/" ++ username ++ "/projects"
  requestParams = mconcat
    [ "?pagination=keyset"
    , "&per_page=50"
    , "&order_by=id"
    , "&sort=asc"
    , "&private_token="
    ]
  url username = maybe
    (projectUrl username)
    ((projectUrl username ++ requestParams) ++)
    accessToken
  privateUrl token username repo
    =  "https://gitlab-ci-token:" <> token <> "@gitlab.com/"
    <> username <> "/" <> repo <> ".git"

  getProjects' username = liftIO $ go $ url username
   where
    go link = do
      resp <- get link
      let result = resp ^.. responseBody . values
      maybe (pure result) (fmap (result ++) . go) $ linkHeader resp
  projectInFolder' value =
    maybe (pure False) folderExists (value ^? key "path" . _String)
  cloneProject' value = case (accessToken, namespace, repoName) of
    (Just token, Just user, Just repo) ->
      cloneGitProject $ privateUrl (T.pack token) user repo
    _ -> maybe (pure ()) cloneGitProject urlValue
   where
    namespace = value ^? key "namespace" . key "path" . _String
    repoName = value ^? key "path" . _String
    urlValue = value ^? key "http_url_to_repo" . _String
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

linkHeader :: Response a -> Maybe String
linkHeader value = trim . fst . flip splitAt resp <$> elemIndex ';' resp
 where
  resp = BS.unpack $ value ^. responseHeader "Links"
  trim []  = []
  trim [_] = []
  trim xs  = tail (init xs)

main :: IO ()
main = do
  homeDir <- getCurrentDirectory
  gitlabPath <- createFolderIfNotExists homeDir "gitlab"
  let secretsDir  = homeDir </> "secrets" </> "secrets.json"
  tokensJson <- fromMaybe Null <$> decodeFileStrict' secretsDir
  let gitlabToken = T.unpack <$> tokensJson ^? key "gitlab_token" . _String
  flip runReaderT gitlabPath $
    updateProjects (gitlabDownloader gitlabToken) (ParN 10) username
 where username = "DarinM223"
