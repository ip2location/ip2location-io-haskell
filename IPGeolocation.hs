{-# LANGUAGE DeriveGeneric,OverloadedStrings,DuplicateRecordFields #-}
{-|
Module      : IPGeolocation
Description : IP2Location.io Haskell package
Copyright   : (c) IP2Location, 2025
License     : MIT
Maintainer  : sales@ip2location.com
Stability   : experimental

This module allows users to query an IP address to get geolocation & proxy info.

IP2Location.io API subscription at https://www.ip2location.io
-}
module IPGeolocation (IPResult(..), ResponseObj(..), ErrorObj(..), ErrorInfo(..), Continent(..), Translation(..), Country(..), Currency(..), Language(..), Region(..), City(..), TimeZoneInfo(..), GeoTargeting(..), ProxyObj(..), lookUpIP) where

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

-- | Translation
data Translation = Translation {
    lang :: Maybe String,
    value :: Maybe String
} deriving (Show, Generic)

-- | Continent
data Continent = Continent {
    name :: String,
    code :: String,
    hemisphere :: [String],
    translation :: Translation
} deriving (Show, Generic)

-- | Currency
data Currency = Currency {
    code :: String,
    name :: String,
    symbol :: String
} deriving (Show, Generic)

-- | Language
data Language = Language {
    code :: String,
    name :: String
} deriving (Show, Generic)

-- | Country
data Country = Country {
    name :: String,
    alpha3_code :: String,
    numeric_code :: Int,
    demonym :: String,
    flag :: String,
    capital :: String,
    total_area :: Int,
    population :: Int,
    currency :: Currency,
    language :: Language,
    tld :: String,
    translation :: Translation
} deriving (Show, Generic)

-- | Region
data Region = Region {
    name :: String,
    code :: String,
    translation :: Translation
} deriving (Show, Generic)

-- | City
data City = City {
    name :: String,
    translation :: Translation
} deriving (Show, Generic)

-- | Time zone info
data TimeZoneInfo = TimeZoneInfo {
    olson :: String,
    current_time :: String,
    gmt_offset :: Int,
    is_dst :: Bool,
    sunrise :: String,
    sunset :: String
} deriving (Show, Generic)

-- | Geotargeting
data GeoTargeting = GeoTargeting {
    metro :: Maybe String
} deriving (Show, Generic)

-- | Proxy
data ProxyObj = ProxyObj {
    last_seen :: Int,
    proxy_type :: String,
    threat :: String,
    provider :: String,
    is_vpn :: Bool,
    is_tor :: Bool,
    is_data_center :: Bool,
    is_public_proxy :: Bool,
    is_web_proxy :: Bool,
    is_web_crawler :: Bool,
    is_residential_proxy :: Bool,
    is_consumer_privacy_network :: Bool,
    is_enterprise_private_network :: Bool,
    is_spammer :: Bool,
    is_scanner :: Bool,is_botnet :: Bool
} deriving (Show, Generic)

-- | Main response type
data ResponseObj = ResponseObj {
    ip :: String,
    country_code :: String,
    country_name :: String,
    region_name :: String,
    city_name :: String,
    latitude :: Float,
    longitude :: Float,
    zip_code :: String,
    time_zone :: String,
    asn :: String,
    as :: String,
    isp :: Maybe String,
    domain :: Maybe String,
    net_speed :: Maybe String,
    idd_code :: Maybe String,
    area_code :: Maybe String,
    weather_station_code :: Maybe String,
    weather_station_name :: Maybe String,
    mcc :: Maybe String,
    mnc :: Maybe String,
    mobile_brand :: Maybe String,
    elevation :: Maybe Int,
    usage_type :: Maybe String,
    address_type :: Maybe String,
    continent :: Maybe Continent,
    country :: Maybe Country,
    region :: Maybe Region,
    city :: Maybe City,
    time_zone_info :: Maybe TimeZoneInfo,
    geotargeting :: Maybe GeoTargeting,
    ads_category :: Maybe String,
    ads_category_name :: Maybe String,
    district :: Maybe String,
    is_proxy :: Bool,
    fraud_score :: Maybe Int,
    proxy :: Maybe ProxyObj
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
data IPResult
    = IPResponse ResponseObj
    | IPError ErrorObj
    deriving (Show, Generic)

-- Derive FromJSON instances
instance FromJSON Translation
instance FromJSON Continent
instance FromJSON Currency
instance FromJSON Language
instance FromJSON Country
instance FromJSON Region
instance FromJSON City
instance FromJSON TimeZoneInfo
instance FromJSON GeoTargeting
instance FromJSON ProxyObj
instance FromJSON ResponseObj
instance FromJSON ErrorInfo
instance FromJSON ErrorObj
instance FromJSON IPResult where
    parseJSON v = 
        (IPResponse <$> parseJSON v) <|> (IPError <$> parseJSON v)

{-|
    The 'lookUpIP' function returns an IPResult containing geolocation & proxy data for an IP address
    It takes 3 arguments; the API configuration, either IPv4 or IPv6 address (String), lang
-}
lookUpIP :: Config -> String -> String -> IO IPResult
lookUpIP myconfig ip lang = do
    let format = "json"
    let mySource = source myconfig
    let myVersion = version myconfig
    let myKey = apiKey myconfig

    let langStr = if lang == ""
        then ""
        else "&lang=" ++ (URIE.encode lang)

    let url = "https://api.ip2location.io/?key=" ++ (URIE.encode myKey) ++ "&source=" ++ mySource ++ "&source-version=" ++ myVersion ++ "&format=" ++ format ++ "&ip=" ++ (URIE.encode ip) ++ langStr
    manager <- newManager tlsManagerSettings
    httprequest <- parseRequest $ url
    httpresponse <- httpLbs httprequest manager
    let json = responseBody httpresponse

    case eitherDecode json of
        Right result -> return result
        Left err -> die("ERROR: " ++ err)
