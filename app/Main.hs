{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Aeson hiding (json)
import Data.Aeson.QQ
import Data.Monoid ((<>))
import Web.Scotty
import Data.Maybe
import Data.List
import Data.Grendy.BigQuery
import Data.Grendy.GraphQL

-- | This function takes a list of (GitHub) repos and a list of (bigquery)
-- repo name / today stars pairs and applies the todays stars data from
-- big query to the github repos. In other words, add the number of todays
-- stars to each repo, since this data is not provided by GitHub.
applyTodayStars  :: [Repo] -> [TodayStar] -> [Repo]
applyTodayStars repo lang = catMaybes $ applyTodayStars' repo <$> lang
  where
    applyTodayStars' repo lang =
      case find (\r -> nameWithOwner r == getName lang) repo of
             Just r' -> Just r' { stars = Just (getStars lang) }
             Nothing -> Nothing

ghQuery :: String -> Value
ghQuery repos = [aesonQQ|
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

    "operationName": null
}
|]

bigQuerySQL :: String
bigQuerySQL =
     "SELECT events.repo.name AS repo,\
    \ COUNT(DISTINCT events.actor.id) AS stars\
    \ FROM ( SELECT * FROM [githubarchive:day.20170920]) AS events\
    \ WHERE events.type = 'WatchEvent'\
    \ GROUP BY 1 ORDER BY 2 DESC LIMIT 1000"

starsToString :: TodayStars -> String
starsToString stars = "repo:" <> starsToString' stars
    where
      starsToString' stars' = intercalate " repo:"
        . take 100
        . fmap getName
        $ rows stars'

main :: IO ()
main = do
  bigQueryResult <- checkAPIError <$> bigQuery bigQuerySQL
  let graphQL = graphQuery . ghQuery $ starsToString bigQueryResult
  res <- checkAPIError <$> graphQL
  let languages = rows bigQueryResult
  let repos = node <$> res
  scotty 3000 . get "" . json $ applyTodayStars repos languages
  where
    checkAPIError (Right b) = b
    checkAPIError (Left err) = error err
