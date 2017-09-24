{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module Main where

import Web.Scotty
import Network.HTTP.Conduit hiding (setRequestBodyLB)
import Network.HTTP.Simple hiding (httpLbs)
import Data.Aeson.QQ
import Data.Aeson (Value)
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.ByteString.Lazy as Lazy
import Network.HTTP.Types.Header
import Data.Monoid ((<>))
import System.Environment (getEnv)

import Data.String.Conversions (cs)


httpPost :: String -> String -> IO Lazy.ByteString
httpPost url post = do
  initReq <- parseUrl url
  let req = setRequestBodyLBS (fromString post)
                    $ initReq { secure = True
                              , method = "POST"
                              }
  res <- withManager $ httpLbs req
  pure $ responseBody res

bigQuery :: String -> IO Lazy.ByteString
bigQuery = httpPost "https://pyapi-vida.herokuapp.com/bigquery"

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
  res <- githubGraphQL graphQuery
  print res
  scotty 3000 $ do
    get "/trends" $ raw res
    get "/test" $ raw result

