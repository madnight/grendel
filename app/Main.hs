{-# LANGUAGE
   OverloadedStrings
 , QuasiQuotes
 , FlexibleContexts
 , DeriveGeneric
#-}

module Main where

import Data.Aeson (Value, FromJSON, ToJSON, eitherDecode)
import Data.Aeson.QQ
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Monoid ((<>))
import System.Environment (getEnv)
import Web.Scotty
import qualified Data.ByteString.Lazy as Lazy
import Data.String.Conversions (cs)
import GHC.Generics
import qualified Network.Wreq as W
import Control.Lens
import Data.Maybe

data Language = Language String Int
  deriving (Show, Generic)

data Languages =
  Languages { error :: Bool
            , rows :: [Language]
            } deriving (Show, Generic)

instance FromJSON Languages
instance ToJSON Languages

instance FromJSON Language
instance ToJSON Language

-- | Either contains decode error message or parsed Languages
bigQuery :: String -> IO (Either String Languages)
bigQuery query = do
    r <- W.post "https://pyapi-vida.herokuapp.com/bigquery" (fromString query)
    pure . eitherDecode $ fromMaybe "Empty Response" (r ^? W.responseBody)


githubGraphQL :: Value -> IO Lazy.ByteString
githubGraphQL query = do
    token <- getEnv "GITHUB_API_TOKEN"
    r <- W.postWith (header token) "https://api.github.com/graphql" query
    pure (r ^. W.responseBody)
      where header token = W.defaults
              & W.header "Authorization" .~ [cs ("bearer " <> token)]
              & W.header "User-Agent" .~ ["Haskell Network.HTTP.Client"]


graphQuery :: Value
graphQuery = [aesonQQ|
{
  "query": "fragment repository on Repository {
        nameWithOwner
        createdAt
        description
        license
        primaryLanguage {
            name
        }
        stargazers {
            totalCount
        }
        owner {
          avatarUrl
        }
    }
    query {
        a: repository(owner: \"facebook\", name: \"react\") {
            ...repository
        }
        b: repository(owner: \"madnight\", name: \"githut\") {
            ...repository
        }
    }",
   "variables":null
}
|]

bigQuerySQL :: String
bigQuerySQL =
     "SELECT events.repo.name AS repo,\
    \ COUNT(DISTINCT events.actor.id) AS stars\
    \ FROM ( SELECT * FROM [githubarchive:day.20170920]) AS events\
    \ WHERE events.type = 'WatchEvent'\
    \ GROUP BY 1 ORDER BY 2 DESC LIMIT 1000"


main :: IO ()
main = do
  result <- bigQuery bigQuerySQL
  print result
  res <- githubGraphQL graphQuery
  scotty 3000 $ do
    get "/trends" $ raw res
    {- get "/test" $ raw result -}
