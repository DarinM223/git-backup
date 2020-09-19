{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Conduit
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Zip (mzip)
import Control.Scheduler
import Data.Aeson
import Data.Aeson.Lens
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Simple (getResponseBody, httpSource, parseRequest)
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
  baseUrl = "https://gitlab.com/api/v4"
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

githubBaseUrl :: String
githubBaseUrl = "https://api.github.com"

githubRetrievePages
  :: MonadIO m => String -> Maybe String -> String -> m [Value]
githubRetrievePages username accessToken = liftIO . go
 where
  opts = case accessToken of
    Just token ->
      defaults & auth ?~ basicAuth (BS.pack username) (BS.pack token)
    Nothing -> defaults
  go link = do
    resp <- getWith opts link
    let next   = BS.unpack <$> resp ^? responseLink "rel" "next" . linkURL
        result = resp ^.. responseBody . values
    maybe (pure result) (fmap (result ++) . go) next

githubDownloader :: Maybe String -> Downloader (ReaderT FilePath IO) Value
githubDownloader accessToken = Downloader
  { getProjects     = getProjects'
  , projectInFolder = projectInFolder'
  , cloneProject    = cloneProject'
  , pullProject     = pullProject'
  }
 where
  privateUrl token username repo =
    "https://" <> username <> ":" <> token <> "@github.com/" <> repo <> ".git"

  getProjects' username =
    githubRetrievePages username accessToken $ githubBaseUrl ++ "/user/repos"
  projectInFolder' value =
    maybe (pure False) folderExists (value ^? key "name" . _String)
  cloneProject' value = case (accessToken, namespace, repoName) of
    (Just token, Just user, Just repo) ->
      cloneGitProject $ privateUrl (T.pack token) user repo
    _ -> maybe (pure ()) cloneGitProject urlValue
   where
    namespace = value ^? key "owner" . key "login" . _String
    repoName = value ^? key "full_name" . _String
    urlValue = value ^? key "html_url" . _String
  pullProject' value =
    maybe (pure ()) pullGitProject (value ^? key "name" . _String)

gistDownloader :: Maybe String -> Downloader (ReaderT FilePath IO) Value
gistDownloader accessToken = Downloader
  { getProjects     = getProjects'
  , projectInFolder = projectInFolder'
  , cloneProject    = cloneProject'
  , pullProject     = pullProject'
  }
 where
  getProjects' username =
    githubRetrievePages username accessToken $ githubBaseUrl ++ "/gists"
  projectInFolder' value =
    maybe (pure False) folderExists (value ^? key "id" . _String)
  cloneProject' value = ask >>= \rootPath ->
    traverse_
      (liftIO . removeFolderIfExists . (rootPath </>) . T.unpack)
      (value ^? key "id" . _String)
   where
    removeFolderIfExists path =
      doesPathExist path >>= flip when (removeDirectoryRecursive path)
  pullProject' value = ask >>= \rootPath ->
    forM_ (value ^? key "id" . _String) $ \gistId -> do
      let path = rootPath </> T.unpack gistId
      liftIO $ createDirectoryIfMissing False path
      traverse_
        (liftIO . downloadFile path)
        (value ^.. key "files" . members . _Value)
   where
    downloadFile path v = do
      let filenameMaybe = T.unpack <$> v ^? key "filename" . _String
          rawUrlMaybe   = T.unpack <$> v ^? key "raw_url" . _String
          filepathMaybe = (path </>) <$> filenameMaybe
      forM_ (mzip rawUrlMaybe filepathMaybe) $ \(rawUrl, filepath) -> do
        request <- parseRequest rawUrl
        runConduitRes $ httpSource request getResponseBody .| sinkFile filepath

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
  let gitlabPath = homeDir </> "gitlab"
      githubPath = homeDir </> "github"
      gistsPath  = homeDir </> "gists"
  createDirectoryIfMissing False gitlabPath
  createDirectoryIfMissing False githubPath
  createDirectoryIfMissing False gistsPath
  let secretsDir  = homeDir </> "secrets" </> "secrets.json"
  tokensJson <- fromMaybe Null <$> decodeFileStrict' secretsDir
  let gitlabToken = T.unpack <$> tokensJson ^? key "gitlab_token" . _String
      githubToken = T.unpack <$> tokensJson ^? key "github_token" . _String
  flip runReaderT gistsPath $
    updateProjects (gistDownloader githubToken) (ParN 1) username
  --flip runReaderT githubPath $
  --  updateProjects (githubDownloader githubToken) (ParN 1) username
  --flip runReaderT gitlabPath $
  --  updateProjects (gitlabDownloader gitlabToken) (ParN 10) username
 where username = "DarinM223"
