module Main where

import Data.Char
import Network.HTTP
import Data.Maybe
import Control.Concurrent

import System.IO
import Control.Monad

import Data.Time.Clock
import Data.Time.Calendar

import Data.List.Split

import System.Environment
import System.Directory

import ResultsParsing
import Geocoding



main :: IO ()
main =
    sequence_ $ fmap processDate ["properties"]

processDate :: String -> IO ()
processDate date = do
    allFiles <- listFiles date
    allProperties <- mapM fileToProperties allFiles
    let flattenedPropertiesWithPrice = filter hasPrice $ concat allProperties
    geocodedProperties <- propertiesWithGeocoding flattenedPropertiesWithPrice
    print geocodedProperties

propertiesWithGeocoding :: [ParsedProperty] -> IO [(ParsedProperty, Maybe LatLng)]
propertiesWithGeocoding properties = do
    let batchProperties = chunksOf 100 properties
    batchGeocodedLocations <- mapM geocodeAddresses batchProperties
    let geocodedLocations = fromJust $ concat <$> sequence batchGeocodedLocations
    return geocodedLocations

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
