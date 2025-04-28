{-# LANGUAGE DeriveGeneric,OverloadedStrings,DuplicateRecordFields #-}
{-|
Module      : HostedDomain
Description : IP2Location.io Haskell package
Copyright   : (c) IP2Location, 2025
License     : MIT
Maintainer  : sales@ip2location.com
Stability   : experimental

This module allows users to query an IP address to get hosted domains info.

IP2Location.io API subscription at https://www.ip2location.io
-}
module HostedDomain (HostedResult(..), ResponseObj(..), ErrorObj(..), ErrorInfo(..), lookUpHosted) where

import Control.Exception
import System.Exit
import Data.Aeson as DA
import Data.Aeson.Types (Result(..), Parser)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Network.URI.Encode as URIE
import Configuration
import GHC.Generics (Generic)
import Control.Applicative ((<|>))

-- | Main response type
data ResponseObj = ResponseObj {
    ip :: String,
    total_domains :: Int,
    page :: Int,
    per_page :: Int,
    total_pages :: Int,
    domains :: [String]
} deriving (Show, Generic)

-- | Define the error detail structure
data ErrorInfo = ErrorInfo {
    error_code :: Int,
    error_message :: String
} deriving (Show, Generic)

-- | Define the error structure
data ErrorObj = ErrorObj {
    error :: ErrorInfo
} deriving (Show, Generic)

-- | Define the wrapper type for Response or Error
data HostedResult
    = HostedResponse ResponseObj
    | HostedError ErrorObj
    deriving (Show, Generic)

-- Derive FromJSON instances
instance FromJSON ResponseObj
instance FromJSON ErrorInfo
instance FromJSON ErrorObj
instance FromJSON HostedResult where
    parseJSON v = 
        (HostedResponse <$> parseJSON v) <|> (HostedError <$> parseJSON v)

{-|
    The 'lookUpHosted' function returns an HostedResult containing hosted domains data for an IP address
    It takes 3 arguments; the API configuration, either IPv4 or IPv6 address (String), page
-}
lookUpHosted :: Config -> String -> Int -> IO HostedResult
lookUpHosted myconfig ip page = do
    let format = "json"
    let mySource = source myconfig
    let myVersion = version myconfig
    let myKey = apiKey myconfig

    let url = "https://domains.ip2whois.com/domains?key=" ++ (URIE.encode myKey) ++ "&source=" ++ mySource ++ "&source-version=" ++ myVersion ++ "&format=" ++ format ++ "&ip=" ++ (URIE.encode ip) ++ "&page=" ++ show page
    manager <- newManager tlsManagerSettings
    httprequest <- parseRequest $ url
    httpresponse <- httpLbs httprequest manager
    let json = responseBody httpresponse

    case eitherDecode json of
        Right result -> return result
        Left err -> die("ERROR: " ++ err)
