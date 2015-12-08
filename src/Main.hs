{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Main where

import Data.Aeson
import qualified Data.Map.Lazy as LM
import Data.Text (Text)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit
import System.Environment
import System.Process

data File =
  File { filename :: String
       , language :: Maybe String
       , raw_url :: String
       } deriving (Show, Generic, FromJSON)

newtype Files = Files (LM.Map Text File) deriving (Show, Generic, FromJSON)
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

getAllFiles :: Gists -> [File]
getAllFiles (Gists []) = []
getAllFiles (Gists (Gist (Files m):xs)) = LM.elems m ++ getAllFiles (Gists xs)

downloadFile :: File -> IO ()
downloadFile (File f l r) = do
  content <- getRequest r
  LBS.writeFile f content

main :: IO ()
main = do
  json <- getRequest sourceURI
  case (decode json :: Maybe Gists) of
    Nothing -> putStrLn "Failed to parse JSON data."
    Just gists -> do
      sequence $ map downloadFile (getAllFiles gists)
      (_, _, _, handle) <- createProcess $ shell "git add ."
      waitForProcess handle
      (_, _, _, handle) <- createProcess $ shell "git commit -m 'Seems a bunch of interesting stuff!'"
      waitForProcess handle
      (_, _, _, handle) <- createProcess $ shell "git push origin master"
      return ()
