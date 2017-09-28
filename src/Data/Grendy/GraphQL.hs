{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Grendy.GraphQL
    ( Repo(..),
      Nodes(..),
      nodes,
      graphQuery,
    ) where

import GHC.Generics
import Data.Aeson hiding (json)
import Control.Applicative (optional)
import Data.Aeson.Types
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Network.Wreq as W
import Data.Maybe (fromMaybe)
import System.Environment (getEnv)
import Control.Lens ((^.), (.~ ), (&))
import Data.String.Conversions (cs)
import Data.Monoid ((<>))
import Control.Monad (mzero)

data Nodes =
  Nodes { node :: Repo } deriving (Show, Generic)

data Repo =
  Repo { nameWithOwner :: String
       , createdAt :: String
       , description :: Maybe String
       , license :: Maybe String
       , name :: Maybe String
       , totalCount :: Int
       , avatarUrl :: String
       , stars :: Maybe Int
       } deriving (Show, Generic)

instance ToJSON Nodes
instance FromJSON Nodes

instance ToJSON Repo
instance FromJSON Repo where
  parseJSON (Object o) =
    Repo <$> o .: "nameWithOwner"
         <*> o .: "createdAt"
         <*> optional (o .: "description")
         <*> optional (o .: "license")
         <*> optional ((o .: "primaryLanguage") >>= (.: "name"))
         <*> ((o .: "stargazers") >>= (.: "totalCount"))
         <*> ((o .: "owner") >>= (.: "avatarUrl"))
         <*> optional (o .: "stars")
  parseJSON _ = mzero

nodes :: Value -> Parser [Nodes]
nodes = withObject mempty
    $ \o -> o .: "data" >>= (.: "search") >>= (.: "edges")

-- | Returns raw github answers
graphQuery :: Value -> IO (Either String [Nodes])
graphQuery query = do
    token <- getEnv "GITHUB_API_TOKEN"
    r <- W.postWith (header token) "https://api.github.com/graphql" query
    pure $ parseEither nodes =<< eitherDecode (r ^. W.responseBody)
      where header token = W.defaults
              & W.header "Authorization" .~ [cs ("bearer " <> token)]
              & W.header "User-Agent" .~ ["Haskell Network.HTTP.Client"]
