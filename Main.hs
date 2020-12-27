{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Conduit
import Control.Concurrent.Async (concurrently_)
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
import System.Environment (getArgs)
import System.Exit (die)
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

type TraverseFn m a = (a -> m ()) -> [a] -> m ()

updateProjects :: Monad m => Downloader m a -> TraverseFn m a -> String -> m ()
updateProjects Downloader{..} traverse' username =
  getProjects username >>= traverse' handleProject
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
      (liftIO . createDirectoryIfMissing False . (rootPath </>) . T.unpack)
      (value ^? key "id" . _String)
  pullProject' value = ask >>= \rootPath -> liftIO $
    forM_ (value ^? key "id" . _String) $ \gistId -> do
      let path        = rootPath </> T.unpack gistId
          updatedPath = path </> T.unpack gistId
      doesFileExist updatedPath >>= flip unless (writeFile updatedPath "")
      updatedFile <- fromMaybe Null <$> decodeFileStrict' updatedPath
      let updated  = updatedFile ^? key "updated_at" . _String
          updated' = value ^? key "updated_at" . _String
      unless (updated == updated') $ do
        putStrLn $ "Pulling changes for: " ++ T.unpack gistId
        traverse_
          (downloadFile path)
          (value ^.. key "files" . members . _Value)
        encodeFile updatedPath value
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
  secretsDir <- getCurrentDirectory
  let secretsPath  = secretsDir </> "secrets" </> "secrets.json"

  homeDir <- getArgs >>= \case
    [dir] -> pure dir
    []    -> getCurrentDirectory
    _     -> die "Invalid number of arguments"
  let gitlabPath = homeDir </> "gitlab"
      githubPath = homeDir </> "github"
      gistsPath  = homeDir </> "gists"
  createDirectoryIfMissing False gitlabPath
  createDirectoryIfMissing False githubPath
  createDirectoryIfMissing False gistsPath
  tokensJson <- fromMaybe Null <$> decodeFileStrict' secretsPath
  let
    gitlabToken    = T.unpack <$> tokensJson ^? key "gitlab_token" . _String
    gitlabUsername = T.unpack <$> tokensJson ^? key "gitlab_username" . _String
    githubToken    = T.unpack <$> tokensJson ^? key "github_token" . _String
    githubUsername = T.unpack <$> tokensJson ^? key "github_username" . _String
  let
    runGithub = forM_ githubUsername $ \username -> do
      flip runReaderT gistsPath $
        updateProjects (gistDownloader githubToken) githubTraverse username
      flip runReaderT githubPath $
        updateProjects (githubDownloader githubToken) githubTraverse username
    runGitlab = flip runReaderT gitlabPath $
      traverse_
        (updateProjects (gitlabDownloader gitlabToken) gitlabTraverse)
        gitlabUsername
  concurrently_ runGithub runGitlab
 where
  githubTraverse = traverseConcurrently_ (ParN 1)
  gitlabTraverse = traverseConcurrently_ (ParN 10)
