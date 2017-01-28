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

import Geocoding

propertiesWithGeocoding :: [String] -> IO [(String, Maybe LatLng)]
propertiesWithGeocoding properties = do
    let batchProperties = chunksOf 100 properties
    batchGeocodedLocations <- mapM geocodeAddresses batchProperties
    let geocodedLocations = fromJust $ concat <$> sequence batchGeocodedLocations
    return geocodedLocations

main :: IO ()
main = do
    contents <- readFile "addresses.txt"
    let addresses = splitOn "\n" contents
    geocoded <- addressesWithGeocoding addresses
    print geocoded

addressesWithGeocoding :: [String] -> IO [(String, Maybe LatLng)]
addressesWithGeocoding addresses = do
    let batchAddresses = chunksOf 100 addresses
    batchGeocodedAddresses <- mapM geocodeAddresses batchAddresses
    let geocodedLocations = fromJust $ concat <$> sequence batchGeocodedAddresses
    return geocodedLocations

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

geocodeAddresses :: [String] -> IO (Maybe [(String, Maybe LatLng)])
geocodeAddresses addresses = do
    mapQuestKey <- getEnv "MAP_QUEST_KEY"
    _ <- print (length addresses)
    geocodeResponse <- openURL $ mapQuestUrl mapQuestKey addresses
    return $ fmap (zip addresses) (geocodeResponseToResults geocodeResponse)
