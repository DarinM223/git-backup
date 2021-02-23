{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Conduit ((.|), runConduitRes, sinkFile)
import Control.Concurrent.Async (concurrently_)
import Control.Lens ((&), (^..), (^?), (^.), (?~))
import Control.Monad (unless, void)
import Control.Monad.Zip (mzip)
import Control.Scheduler (Comp (ParN), traverseConcurrently_)
import Data.Aeson (Value (Null), decodeFileStrict', encodeFile)
import Data.Aeson.Lens
  (AsPrimitive (_String), AsValue (_Value), key, members, values)
import Data.Foldable (for_, traverse_)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Simple (getResponseBody, httpSource, parseRequest)
import Network.Wreq
  ( Response
  , basicAuth
  , get
  , getWith
  , defaults
  , auth
  , linkURL
  , responseBody
  , responseHeader
  , responseLink )
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , doesPathExist
  , getCurrentDirectory )
import System.Process
  (CreateProcess (cwd), createProcess, shell, waitForProcess)
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

gitlabDownloader :: FilePath -> Maybe String -> Downloader IO Value
gitlabDownloader root accessToken = Downloader{..}
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

  getProjects username = go $ url username
   where
    go link = do
      resp <- get link
      let result = resp ^.. responseBody . values
      maybe (pure result) (fmap (result ++) . go) $ linkHeader resp
  projectInFolder value =
    maybe (pure False) (folderExists root) (value ^? key "path" . _String)
  cloneProject value = case (accessToken, namespace, repoName) of
    (Just token, Just user, Just repo) ->
      cloneGitProject root $ privateUrl (T.pack token) user repo
    _ -> maybe (pure ()) (cloneGitProject root) urlValue
   where
    namespace = value ^? key "namespace" . key "path" . _String
    repoName = value ^? key "path" . _String
    urlValue = value ^? key "http_url_to_repo" . _String
  pullProject value =
    maybe (pure ()) (pullGitProject root) (value ^? key "path" . _String)

githubBaseUrl :: String
githubBaseUrl = "https://api.github.com"

githubRetrievePages :: String -> Maybe String -> String -> IO [Value]
githubRetrievePages username accessToken = go
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

githubDownloader :: FilePath -> Maybe String -> Downloader IO Value
githubDownloader root accessToken = Downloader{..}
 where
  privateUrl token username repo =
    "https://" <> username <> ":" <> token <> "@github.com/" <> repo <> ".git"

  getProjects username =
    githubRetrievePages username accessToken $ githubBaseUrl ++ "/user/repos"
  projectInFolder value =
    maybe (pure False) (folderExists root)(value ^? key "name" . _String)
  cloneProject value = case (accessToken, namespace, repoName) of
    (Just token, Just user, Just repo) ->
      cloneGitProject root $ privateUrl (T.pack token) user repo
    _ -> maybe (pure ()) (cloneGitProject root) urlValue
   where
    namespace = value ^? key "owner" . key "login" . _String
    repoName = value ^? key "full_name" . _String
    urlValue = value ^? key "html_url" . _String
  pullProject value =
    maybe (pure ()) (pullGitProject root) (value ^? key "name" . _String)

gistDownloader :: FilePath -> Maybe String -> Downloader IO Value
gistDownloader root accessToken = Downloader{..}
 where
  getProjects username =
    githubRetrievePages username accessToken $ githubBaseUrl ++ "/gists"
  projectInFolder value =
    maybe (pure False) (folderExists root) (value ^? key "id" . _String)
  cloneProject value = traverse_
    (createDirectoryIfMissing False . (root </>) . T.unpack)
    (value ^? key "id" . _String)
  pullProject value = for_ (value ^? key "id" . _String) $ \gistId -> do
    let path        = root </> T.unpack gistId
        updatedPath = path </> T.unpack gistId
    doesFileExist updatedPath >>= flip unless (writeFile updatedPath "")
    updatedFile <- fromMaybe Null <$> decodeFileStrict' updatedPath
    let updated  = updatedFile ^? key "updated_at" . _String
        updated' = value ^? key "updated_at" . _String
    unless (updated == updated') $ do
      putStrLn $ "Pulling changes for: " ++ T.unpack gistId
      traverse_ (downloadFile path) (value ^.. key "files" . members . _Value)
      encodeFile updatedPath value
   where
    downloadFile path v = do
      let filenameMaybe = T.unpack <$> v ^? key "filename" . _String
          rawUrlMaybe   = T.unpack <$> v ^? key "raw_url" . _String
          filepathMaybe = (path </>) <$> filenameMaybe
      for_ (mzip rawUrlMaybe filepathMaybe) $ \(rawUrl, filepath) -> do
        request <- parseRequest rawUrl
        runConduitRes $ httpSource request getResponseBody .| sinkFile filepath

folderExists :: FilePath -> Text -> IO Bool
folderExists path folder = doesPathExist $ path </> T.unpack folder

cloneGitProject :: FilePath -> Text -> IO ()
cloneGitProject cwd url = do
  (_, _, _, p) <- createProcess
    (shell $ "git clone " ++ T.unpack url) { cwd = Just cwd }
  void $ waitForProcess p

pullGitProject :: FilePath -> Text -> IO ()
pullGitProject root folder = do
  (_, _, _, p1) <- createProcess (shell "git fetch --all") { cwd = Just path }
  void $ waitForProcess p1
  (_, _, _, p2) <- createProcess (shell "git pull --all") { cwd = Just path }
  void $ waitForProcess p2
 where path = root </> T.unpack folder

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
    runGithub = for_ githubUsername $ \name -> do
      updateProjects (gistDownloader gistsPath githubToken) traverse_ name
      updateProjects (githubDownloader githubPath githubToken) traverse_ name
    runGitlab = traverse_
      (updateProjects (gitlabDownloader gitlabPath gitlabToken) gitlabTraverse)
      gitlabUsername
  concurrently_ runGithub runGitlab
 where gitlabTraverse = traverseConcurrently_ (ParN 10)
