{-# LANGUAGE OverloadedStrings
            , QuasiQuotes
            , DeriveGeneric
            , RecordWildCards
#-}

module Main where

import Data.Aeson (Value, FromJSON, ToJSON, eitherDecode)
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

data Language =
  Language { name :: String
           , stars :: Int
           } deriving (Show, Generic)

data Languages =
  Languages { error :: Bool
            , rows :: [[String, Int]]
            } deriving (Show, Generic)

{- instance FromJSON Language where -}
{-   parseJSON = withObject "langauge" $ \o -> do -}
{-     name <- o .: "name" -}
{-     age  <- o .: "age" -}
{-     return Person{..} -}

{- instance ToJSON Language -}

instance FromJSON Languages
instance ToJSON Languages

bigQuery :: String -> IO Lazy.ByteString
bigQuery post = do
  initReq <- parseUrl "https://pyapi-vida.herokuapp.com/bigquery"
  let req = setRequestBodyLBS (fromString post)
                    $ initReq { secure = True
                              , method = "POST"
                              }
  res <- withManager $ httpLbs req
  pure $ responseBody res

githubGraphQL :: Value -> IO Lazy.ByteString
githubGraphQL post = do
  initReq <- parseUrl "https://api.github.com/graphql"
  token <- getEnv "GITHUB_API_TOKEN"
  let req = setRequestBodyJSON post
                    $ initReq { secure = True
                              , method = "POST"
                              , requestHeaders = [
                                ( hAuthorization, cs $ "bearer " <> token ),
                                ( hUserAgent, "haskcasl" )
                                ]
                              }
  res <- withManager $ httpLbs req
  print req
  pure $ responseBody res

{- https://github.com/sol/aeson-qq -}
graphQuery :: Value
graphQuery = [aesonQQ|
{"query": "fragment repository on Repository {
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
    "variables":null}
|]

main :: IO ()
main = do
  let query = "SELECT events.repo.name AS repo,\
    \ COUNT(DISTINCT events.actor.id) AS stars\
    \ FROM ( SELECT * FROM [githubarchive:day.20170920]) AS events\
    \ WHERE events.type = 'WatchEvent'\
    \ GROUP BY 1 ORDER BY 2 DESC LIMIT 1000"
  result <- bigQuery query
  let d = eitherDecode result :: Either String Languages
  print d
  res <- githubGraphQL graphQuery
  print res
  scotty 3000 $ do
    get "/trends" $ raw res
    get "/test" $ raw result

