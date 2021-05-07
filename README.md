# grendel
Grendel is the GitHub Trends API

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

# Usage

Usage with compiled Docker Image

```bash
docker run -it -e PORT=3000 -e GITHUB_API_TOKEN=YOUR_TOKEN_HERE -p 3000:3000 madnight/grendel
```

Or compile it

```bash
git clone https://github.com/madnight/grendel.git && cd grendel
export GITHUB_API_TOKEN=YOUR_TOKEN_HERE
export PORT=3000
stack build && stack install && grendel-exe
```

