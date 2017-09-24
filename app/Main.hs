{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Monoid (mconcat)
import Network.HTTP.Conduit hiding (setRequestBodyLB)
import Network.HTTP.Simple hiding (httpLbs)
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.ByteString.Lazy as Lazy

main = do
  initReq <- parseUrl "https://pyapi-vida.herokuapp.com/bigquery"
  let query = fromString "SELECT events.repo.name AS repo,\
  \ COUNT(DISTINCT events.actor.id) AS stars\
  \ FROM ( SELECT * FROM [githubarchive:day.20170920]) AS events\ 
  \ WHERE events.type = 'WatchEvent'\
  \ GROUP BY 1 ORDER BY 2 DESC LIMIT 1000"
  let req = setRequestBodyLBS query $ initReq { secure = True
                                              , method = "POST"
                                              }
  res <- withManager $ httpLbs req
  Lazy.putStr $ responseBody res

