{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Lib
    ( someFunc
    ) where

import qualified GitHub as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Environment           (getArgs)
import Data.Text hiding (map,intercalate, groupBy, concat)
import Data.List (intercalate, groupBy, sortBy)
import Data.Either
import           Servant.API                (BasicAuthData (..))
import Data.ByteString.UTF8 (fromString)

someFunc :: IO ()
someFunc = do
  putStrLn "GitHubCall started.."
  (rName:user:token:_) <- getArgs
  putStrLn $ "Name is " ++ rName
  putStrLn $ "Github account for API call is " ++ user
  putStrLn $ "Github token for api call is " ++ token

  let auth = BasicAuthData (fromString user) (fromString token)

  testGitHubCall auth $ pack rName
  putStrLn ".. call finished."


testGitHubCall :: BasicAuthData -> Text -> IO ()
testGitHubCall auth name =
  (SC.runClientM (GH.getUser (Just "haskell-app") auth name) =<< env) >>= \case

    Left err -> do
      putStrLn $ "ERROR, getting user: " ++ show err
    Right res -> do
      putStrLn $ "User is: " ++ "\n\t" ++ show res

      -- now lets get the users repositories
      (SC.runClientM (GH.getUserRepos (Just "haskell-app") auth name) =<< env) >>= \case
        Left err -> do
          putStrLn $ "ERROR, retrieving repos: " ++ show err
        Right repos -> do
          putStrLn $ "Repositories are:" ++ "\n\t" ++
            intercalate "\n\t" (map (\(GH.GitHubRepo n c b) -> "[" ++ show n ++ ", " ++ show c ++ ", " ++ show b ++ "]") repos)

          -- now lets get the full list of collaborators from repositories
          partitionEithers <$> mapM (getContribs auth name) repos >>= \case

            ([], contribs) ->
              putStrLn $ " Contributors are: " ++ "\n\t" ++
              (intercalate "\n\t" .
                map (\(GH.RepoContributor n c) -> "[" ++ show n ++ "," ++ show c ++ "]") .
                groupContributors $ concat contribs)

            (ers, _)-> do
              putStrLn $ "ERROR, getting contributors: " ++ show ers

              -- returning list of all user commits
              --partitionEithers <$> mapM (getCommits auth name) repos >>= \case

                --([], commits) -> do
                --  putStrLn $ " commits are: " ++ show commits

                --(ers, _)-> do
                --  putStrLn $ "heuston, we have a problem (getting commits): " ++ show ers


  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")

        getContribs :: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.RepoContributor])
        getContribs auth name (GH.GitHubRepo repo _ _) =
          SC.runClientM (GH.getRepoContribs (Just "haskell-app") auth name repo) =<< env

        getCommits :: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.RepoCommit])
        getCommits auth name (GH.GitHubRepo repo _ _) =
          SC.runClientM (GH.getRepoCommit (Just "haskell-app") auth name repo) =<< env

        groupContributors :: [GH.RepoContributor] -> [GH.RepoContributor]
        groupContributors  = sortBy (\(GH.RepoContributor _ c1) (GH.RepoContributor _ c2) ->  compare c1 c2) .
                             map mapfn .
                             groupBy (\(GH.RepoContributor l1 _) (GH.RepoContributor l2 _) ->  l1 == l2)


         where mapfn :: [GH.RepoContributor] -> GH.RepoContributor
               mapfn xs@((GH.RepoContributor l _):_) = GH.RepoContributor l . sum $
                                                       map (\(GH.RepoContributor _ c) -> c)  xs
