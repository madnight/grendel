{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Grendy.BigQuery
    ( TodayStar(..)
    , TodayStars(..)
    , getName
    , getStars
    , bigQuery
    ) where

import GHC.Generics
import Data.Aeson hiding (json)
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Lens ((^?))
import qualified Network.Wreq as W
import Data.Maybe (fromMaybe)

data TodayStar = TodayStar String Int
  deriving (Show, Generic)

data TodayStars =
  TodayStars { rows :: [TodayStar] } deriving (Show, Generic)

instance FromJSON TodayStars
instance ToJSON TodayStars

instance FromJSON TodayStar
instance ToJSON TodayStar

getName :: TodayStar -> String
getName (TodayStar n _) = n

getStars :: TodayStar -> Int
getStars (TodayStar _ n) = n

-- | Either contains decode error message or parsed TodayStars
bigQuery :: String -> IO (Either String TodayStars)
bigQuery query = do
    r <- W.post "https://pyapi-vida.herokuapp.com/bigquery" (fromString query)
    pure . eitherDecode $ fromMaybe "Empty Response" (r ^? W.responseBody)
