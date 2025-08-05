{-|
Module      : Configuration
Description : IP2Location.io Haskell package
Copyright   : (c) IP2Location, 2025
License     : MIT
Maintainer  : sales@ip2location.com
Stability   : experimental

This module configures the API key.

IP2Location.io API subscription at https://www.ip2location.io
-}
module Configuration (Config(..), open) where

-- | Contains the API configuration.
data Config = Config {
    -- | API key
    apiKey :: String,
    -- | Source SDK
    source :: String,
    -- | Version
    version :: String
} deriving (Show, Eq)

{-|
    The 'open' function initializes the API configuration.
    It takes 1 argument; the API key.
-}
open :: String -> IO Config
open apikey = do
    return (Config apikey "sdk-haskell-iplio" "1.3.0")
