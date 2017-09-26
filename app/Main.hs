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


-- | Returns raw github answers
githubGraphQL :: Value -> IO Lazy.ByteString
githubGraphQL query = do
    token <- getEnv "GITHUB_API_TOKEN"
    r <- W.postWith (header token) "https://api.github.com/graphql" query
    pure (r ^. W.responseBody)
      where header token = W.defaults
              & W.header "Authorization" .~ [cs ("bearer " <> token)]
              & W.header "User-Agent" .~ ["Haskell Network.HTTP.Client"]

ghQuery :: Value
ghQuery = [aesonQQ|
{
  "query": "query ($repos: String!) {
      search(first: 100, type: REPOSITORY, query: $repos) {
         edges {
             node {
                 ... on Repository {
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
              }
          }
       }
    }",
    "variables": {
      "repos": #{repos}
    },
    "operationName":null
}
|]
 where
    repos = "repo:facebook/react repo:madnight/githut" :: String


bigQuerySQL :: String
bigQuerySQL =
     "SELECT events.repo.name AS repo,\
    \ COUNT(DISTINCT events.actor.id) AS stars\
    \ FROM ( SELECT * FROM [githubarchive:day.20170920]) AS events\
    \ WHERE events.type = 'WatchEvent'\
    \ GROUP BY 1 ORDER BY 2 DESC LIMIT 1000"

fromRight :: Either a b -> b
fromRight (Right b) = b

langInt :: Language -> Int
langInt (Language a b) = b

langStr :: Language -> String
langStr (Language a b) = a

main :: IO ()
main = do
  result <- bigQuery bigQuerySQL
  print $ fmap langStr (rows $ fromRight result)
  res <- githubGraphQL ghQuery
  scotty 3000 $ do
    get "" $ raw res
    {- get "/test" $ raw result -}
