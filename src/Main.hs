module Main where

import Data.Char
import Network.HTTP
import Data.Maybe
import Control.Concurrent

import System.IO
import Control.Monad

import Data.Time.Clock
import Data.Time.Calendar

-- import Data.List.Split

import System.Environment
import System.Directory

import ResultsParsing
import Geocoding

import Streaming 
import qualified Streaming.Prelude as S

main :: IO ()
main =
    sequence_ $ fmap processDate ["properties"]

processDate :: String -> IO ()
processDate date = do
    allFiles <- listFiles date
    let allProperties = S.mapM fileToProperties $ S.each allFiles
        flattenedPropertiesWithPrice = S.filter hasPrice $ S.concat allProperties
    S.print $  propertiesWithGeocoding flattenedPropertiesWithPrice

-- propertiesWithGeocoding :: [ParsedProperty] -> IO [(ParsedProperty, Maybe LatLng)]
propertiesWithGeocoding
  :: Stream (Of ParsedProperty) IO r
     -> Stream (Of (ParsedProperty, Maybe LatLng)) IO r
propertiesWithGeocoding properties = do
    let batchProperties = S.mapped S.toList $ chunksOf 100 properties
    S.concat $ S.concat $ S.mapM geocodeAddresses batchProperties
    -- concat here flattens a stream of lists of as into a stream of as
    -- and a stream of maybe as into a stream of as
 
    
openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

geocodeAddresses :: [ParsedProperty] -> IO (Maybe [(ParsedProperty, Maybe LatLng)])
geocodeAddresses properties = do
    let addresses = fmap location properties
    mapQuestKey <- getEnv "MAP_QUEST_KEY"
    geocodeResponse <- openURL $ mapQuestUrl mapQuestKey addresses
    return $ fmap (zip properties) (geocodeResponseToResults geocodeResponse)


listFiles :: String -> IO [FilePath]
listFiles folder = do
    files <- listDirectory folder
    return $ fmap (\file -> folder ++ "/" ++ file) files

fileToProperties :: FilePath -> IO [ParsedProperty]
fileToProperties path = do
    contents <- readFile path
    return $ parsePage contents

hasPrice :: ParsedProperty -> Bool
hasPrice property = isJust (price property)
