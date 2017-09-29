{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Grendy.GraphQL
    ( Repo(..),
      graphQuery,
    ) where

import Control.Applicative (optional)
import Control.Lens ((^.), (.~ ), (&))
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import GHC.Generics
import System.Environment (getEnv)
import qualified Network.Wreq as W

data Repo =
  Repo { name :: String
       , date :: String
       , desc :: Maybe String
       , license :: Maybe String
       , language :: Maybe String
       , avatarUrl :: String
       , totalStars :: Int
       , todayStars :: Maybe Int
       } deriving (Show, Generic)

instance ToJSON Repo
instance FromJSON Repo  where
  parseJSON = withObject mempty $ \o  -> do
    let p = (o .: "node" >>=)
    name       <-            p (.: "nameWithOwner")
    date       <-            p (.: "createdAt")
    license    <- optional $ p (.: "license")
    desc       <- optional $ p (.: "description")
    language   <- optional $ p (.: "primaryLanguage") >>= (.: "name")
    avatarUrl  <-            p (.: "owner")           >>= (.: "avatarUrl")
    totalStars <-            p (.: "stargazers")      >>= (.: "totalCount")
    todayStars <- optional $ p (.: "stars")
    return Repo{..}

repoParser :: Value -> Parser [Repo]
repoParser = withObject mempty
    $ \o -> o .: "data" >>= (.: "search") >>= (.: "edges")

-- | Returns a list of GitHub repositories
graphQuery :: Value -> IO (Either String [Repo])
graphQuery query = do
    token <- getEnv "GITHUB_API_TOKEN"
    r <- W.postWith (header token) "https://api.github.com/graphql" query
    pure $ parseEither repoParser =<< eitherDecode (r ^. W.responseBody)
      where header token = W.defaults
              & W.header "Authorization" .~ [cs ("bearer " <> token)]
              & W.header "User-Agent" .~ ["Haskell Network.HTTP.Client"]
