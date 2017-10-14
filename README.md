# grendel
Grendel is the GitHub Trends API for Grendy -> https://github.com/madnight/grendy

![](https://i.imgur.com/jn2FeAN.png)

The API collect the required information such as today stars per github repos and proivdes them as API.
It uses Google BigQuery and Github Graphlql to fetch the information.
The JSON data is parsed with AESON.

Parsing

```haskell
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
```


GraphQL Query
```json
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
    }
}
```

Google Big Query
```sql
    "SELECT events.repo.name AS repo,\
    \ COUNT(DISTINCT events.actor.id) AS stars\
    \ FROM ( SELECT * FROM [githubarchive:day." <> time <> "]) AS events\
    \ WHERE events.type = 'WatchEvent'\
    \ GROUP BY 1 ORDER BY 2 DESC LIMIT 1000"
```
