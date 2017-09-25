{-# LANGUAGE
   OverloadedStrings
 , QuasiQuotes
 , DeriveGeneric
 , RecordWildCards
#-}

module Main where

import Data.Aeson (Value, FromJSON, ToJSON, eitherDecode)
import Data.Aeson.Types
import Data.Aeson.QQ
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Monoid ((<>))
import Network.HTTP.Conduit hiding (setRequestBodyLB)
import Network.HTTP.Simple hiding (httpLbs)
import Network.HTTP.Types.Header
import System.Environment (getEnv)
import Web.Scotty
import qualified Data.ByteString.Lazy as Lazy
import Data.String.Conversions (cs)
import GHC.Generics
import Data.Either

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

-- | Either decode error message or parsed Languages
bigQuery :: String -> IO (Either String Languages)
bigQuery post = do
  initReq <- parseUrl "https://pyapi-vida.herokuapp.com/bigquery"
  res <- withManager
    . httpLbs
    . setRequestBodyLBS (fromString post)
    $ initReq { secure = True
              , method = "POST"
              }
  print res
  pure . eitherDecode $ responseBody res

githubGraphQL :: Value -> IO Lazy.ByteString
githubGraphQL post = do
  initReq <- parseUrl "https://api.github.com/graphql"
  token <- getEnv "GITHUB_API_TOKEN"
  res <- withManager
    . httpLbs
    . setRequestBodyJSON post
    $ initReq { secure = True
              , method = "POST"
              , requestHeaders = [
                ( hAuthorization, cs $ "bearer " <> token ),
                ( hUserAgent, "haskcasl" )
                ]
              }
  pure $ responseBody res

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
