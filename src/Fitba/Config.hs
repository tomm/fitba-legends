{-# LANGUAGE OverloadedStrings     #-}
module Fitba.Config (load, Config(..)) where

import Data.Yaml
import Data.Monoid (mempty)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding

data Config = Config { liveDb :: BS.ByteString, testDb :: BS.ByteString } deriving (Show)
instance FromJSON Config where
  parseJSON (Object o) = mkConfig
    <$> o .: "liveDb"
    <*> o .: "testDb"
  parseJSON _ = mempty

mkConfig :: String -> String -> Config
mkConfig live test = Config (Data.Text.Encoding.encodeUtf8 (T.pack live))
                            (Data.Text.Encoding.encodeUtf8 (T.pack test))

load :: IO Config
load =
  (decodeFile "db.yml" :: IO (Maybe Config)) >>= \maybeConfig ->
    pure $ fromMaybe (error "Could not open db.yml") maybeConfig
