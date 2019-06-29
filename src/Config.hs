{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..)
  , Display (..)
  , Social (..)
  , loadConfiguration
  ) where

import Data.Yaml (FromJSON (..), Value (..), (.:), (.:?))
import Data.Yaml.Config (loadYamlSettings, useEnv)

data Config = Config
  { title       :: String
  , description :: String
  , baseUrl     :: String
  , display     :: Display
  , social      :: Social
  } deriving (Eq, Show)

data Display = Display
  { postsPerPage :: Int
  , showFeed     :: Bool
  , showEmail    :: Bool
  , showGithub   :: Bool
  , showTwitter  :: Bool
  } deriving (Eq, Show)

data Social = Social
  { author  :: String
  , email   :: String
  , github  :: Maybe String
  , twitter :: Maybe String
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .: "title"
    <*> v .: "description"
    <*> v .: "base_url"
    <*> v .: "display"
    <*> v .: "social"
  parseJSON _ = undefined

instance FromJSON Display where
  parseJSON (Object v) = Display
    <$> v .: "posts_per_page"
    <*> v .: "feed"
    <*> v .: "email"
    <*> v .: "github"
    <*> v .: "twitter"
  parseJSON _ = undefined

instance FromJSON Social where
  parseJSON (Object v) = Social
    <$> v .:  "author"
    <*> v .:  "email"
    <*> v .:? "github"
    <*> v .:? "twitter"
  parseJSON _ = undefined

loadConfiguration :: IO Config
loadConfiguration = loadYamlSettings ["site/config.yaml"] [] useEnv
