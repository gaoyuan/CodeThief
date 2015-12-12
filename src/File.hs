{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module File where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Lazy as LM
import System.Directory
import System.Environment
import System.Process

data File =
  File { filename :: String
       , language :: Maybe String
       , raw_url :: String
       } deriving (Show, Generic, FromJSON)

newtype Files = Files (LM.Map String File) deriving (Show, Generic, FromJSON)
newtype Gist = Gist Files deriving Show
newtype Gists = Gists [Gist] deriving (Show, Generic, FromJSON)

instance FromJSON Gist where
  parseJSON (Object v) = Gist <$> v .: "files"

sourceURI :: String
sourceURI = "https://api.github.com/gists/public"

getRequest :: String -> IO LBS.ByteString
getRequest url = do
  request <- liftIO $ parseUrl url
  let requestWithHeaders = request { requestHeaders = [("User-Agent", "CodeThief")] }
  manager <- liftIO $ newManager tlsManagerSettings
  response <- httpLbs requestWithHeaders manager
  return $ responseBody response

-- extract all files from a list of gists
filesFromGists :: Gists -> [File]
filesFromGists (Gists []) = []
filesFromGists (Gists (Gist (Files m):xs)) = LM.elems m ++ filesFromGists (Gists xs)

-- download a file to local directory
downloadFile :: File -> String -> IO ()
downloadFile (File f l r) path = do
  content <- getRequest r
  let filePath = path ++ "/" ++ f
  LBS.writeFile filePath content
  putStrLn $ f ++ " downloaded."

-- extract files from json and download them to disk
pullFilesToDisk :: IO ()
pullFilesToDisk = do
  json <- getRequest sourceURI
  case (decode json :: Maybe Gists) of
    Nothing -> putStrLn "Failed to parse JSON data."
    Just gists -> do
      curTime <- getCurrentTime
      let dirName = formatTime defaultTimeLocale "%F-%H-%M-%S" curTime
      createDirectoryIfMissing False dirName
      mapConcurrently (`downloadFile` dirName) (filesFromGists gists)
      putStrLn "All files downloaded successfully!"
