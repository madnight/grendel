{-# LANGUAGE
   OverloadedStrings
 , QuasiQuotes
 , FlexibleContexts
 , DeriveGeneric
#-}

module Main where

{- import Data.Aeson (Value, FromJSON, ToJSON, eitherDecode, withObject, (.:), decode, ) -}
import Data.Aeson
import Data.Aeson.QQ
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Monoid ((<>))
import System.Environment (getEnv)
import Web.Scotty
import Data.String.Conversions (cs)
import GHC.Generics
import qualified Network.Wreq as W
import Control.Lens
import Data.Maybe
import Data.List
import Data.Aeson.Types
import Control.Monad (mzero)
import Control.Applicative (optional)

data Language = Language String Int
  deriving (Show, Generic)


getName (Language n _) = n

data Languages =
  Languages { error :: Bool
            , rows :: [Language]
            } deriving (Show, Generic)

instance FromJSON Languages
instance ToJSON Languages

instance FromJSON Language
instance ToJSON Language

data Repo =
  Repo { nameWithOwner :: String
       , createdAt :: String
       , description :: Maybe String
       , license :: Maybe String
       , name :: Maybe String
       , totalCount :: Int
       , avatarUrl :: String
       , stars :: Maybe String
       } deriving (Show, Generic)

addStars :: Repo -> Language -> Repo
addStars repo lang =
  if (nameWithOwner repo) == (getName lang)
    then repo { stars = Just (getName lang) }
    else repo

data Nodes =
  Nodes { node :: Repo } deriving (Show, Generic)

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

{- instance FromJSON Repo -}
{- instance ToJSON Repo -}
instance FromJSON Nodes
instance ToJSON Nodes

instance ToJSON Repo

{- data GithubRepositories = -}
  {- GithubRepositories { rows_ :: [Language] -}
                     {- } deriving (Show, Generic) -}



-- | Either contains decode error message or parsed Languages
bigQuery :: String -> IO (Either String Languages)
bigQuery query = do
    r <- W.post "https://pyapi-vida.herokuapp.com/bigquery" (fromString query)
    pure . eitherDecode $ fromMaybe "Empty Response" (r ^? W.responseBody)


-- | Returns raw github answers
githubGraphQL :: Value -> IO (Either String [Nodes])
githubGraphQL query = do
    token <- getEnv "GITHUB_API_TOKEN"
    r <- W.postWith (header token) "https://api.github.com/graphql" query
    pure $ parseEither nodes =<< eitherDecode (r ^. W.responseBody)
    {- pure . eitherDecode $ fromMaybe "Empty Response" $ parseMaybe (nodes =<< decode) (r ^. W.responseBody) -}
      where header token = W.defaults
              & W.header "Authorization" .~ [cs ("bearer " <> token)]
              & W.header "User-Agent" .~ ["Haskell Network.HTTP.Client"]

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

fromRight :: Either a b -> b
fromRight (Right b) = b

langInt :: Language -> Int
langInt (Language a b) = b

getLangName :: Language -> String
getLangName (Language a b) = a

rep :: Either String Languages -> String
rep result = "repo:"
    <> (intercalate " repo:"
    $ take 100
    $ fmap getLangName (rows $ fromRight result))

main :: IO ()
main = do
  result <- bigQuery bigQuerySQL
  print result
  print $ rep result
  res <- githubGraphQL (ghQuery (rep result))
  print res
  {- scotty 3000 $ do -}
    {- get "" $ raw res -}
    {- get "/test" $ raw result -}
