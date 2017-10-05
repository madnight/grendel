{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Grendy.BigQuery
    ( TodayStar(..)
    , getName
    , getStars
    , bigQuery
    ) where

import Control.Lens ((^.), (.~ ), (&))
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.ByteString.Lazy.UTF8 (fromString)
import GHC.Generics
import qualified Network.Wreq as W
import Data.Maybe (fromMaybe)

data TodayStar = TodayStar String Int
  deriving (Show, Generic)

instance FromJSON TodayStar
instance ToJSON TodayStar

getName :: TodayStar -> String
getName (TodayStar n _) = n

getStars :: TodayStar -> Int
getStars (TodayStar _ n) = n

starParser :: Value -> Parser [TodayStar]
starParser = withObject mempty
    $ \o -> o .: "rows"

-- | Either contains decode error message or parsed TodayStars
bigQuery :: String -> IO (Either String [TodayStar])
bigQuery query = do
    r <- W.post "https://pyapi-vida.herokuapp.com/bigquery" (fromString query)
    pure $ parseEither starParser =<< eitherDecode  (r ^. W.responseBody)
