{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module GitHub where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

type Username  = Text
type UserAgent = Text
type Reponame  = Text

-- Github user info data retrieved
data GitHubUser =
  GitHubUser { login :: Text
             , name  :: Maybe Text
             , email :: Maybe Text
             , public_repos :: Integer
             , location :: Maybe Text
             } deriving (Generic, FromJSON, Show)

-- GitHub user repo info
data GitHubRepo =
  GitHubRepo { name :: Text
             , full_name :: Maybe Text
             , language :: Maybe Text
             } deriving (Generic, FromJSON, Show)

-- Github user repo contributors
data RepoContributor =
  RepoContributor { login :: Text
                  , contributions :: Integer
                  } deriving (Generic, FromJSON, Show)

-- Github user repo commits
data RepoCommit =
  RepoCommits { url :: Text
              , commit :: Maybe Object
              } deriving (Generic, FromJSON, Show)

-- Call enpoints
type GitHubAPI = -- User info
                 "users" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username  :> Get '[JSON] GitHubUser

                -- User repositories
            :<|> "users" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username  :> "repos" :>  Get '[JSON] [GitHubRepo]

                -- User repositories' contributors
            :<|> "repos" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username
                         :> Capture "repo"     Reponame  :> "contributors" :>  Get '[JSON] [RepoContributor]

                -- User repositories' commits
            :<|> "repos" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username
                         :> Capture "repo"     Reponame  :> "commits" :>  Get '[JSON] [RepoCommit]

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

getUser ::          Maybe UserAgent -> BasicAuthData -> Username            -> ClientM GitHubUser
getUserRepos ::     Maybe UserAgent -> BasicAuthData -> Username            -> ClientM [GitHubRepo]
getRepoContribs ::  Maybe UserAgent -> BasicAuthData -> Username -> Reponame -> ClientM [RepoContributor]
getRepoCommit ::  Maybe UserAgent -> BasicAuthData -> Username -> Reponame -> ClientM [RepoCommit]

getUser :<|> getUserRepos :<|> getRepoContribs :<|> getRepoCommit = client gitHubAPI
