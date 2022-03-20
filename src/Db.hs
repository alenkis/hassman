{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- In order to get a version of HLS that supports TemplateHaskell pragma,
-- steps outline here need to followed: https://github.com/haskell/haskell-language-server/issues/2659#issuecomment-1048984537

-- |
module Db where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Text
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share
  [ mkMigrate "migrateAll",
    mkPersist sqlSettings
  ]
  [persistLowerCase|
Password
    url String
    Domain url
    username String
    password String
    deriving Show
|]

dbName :: Text
dbName = "db:memory"

createPassword :: String -> String -> String -> IO ()
createPassword url username password = runSqlite dbName $ do
  runMigration migrateAll
  pId <- insert $ Password url username password
  p <- get pId
  case p of
    Nothing -> error "Something went wrong"
    Just _ -> liftIO $ print $ "Successfully added password for domain: " ++ url

getPassword :: String -> String -> IO ()
getPassword domain username = runSqlite dbName $ do
  runMigration migrateAll
  password <- getBy $ Domain domain
  case password of
    Nothing -> liftIO $ print $ "Could not find password for domain: " ++ domain
    Just p -> liftIO $ print $ "password: " ++ show (passwordPassword $ entityVal p)

listPasswords :: IO ()
listPasswords = runSqlite dbName $ do
  runMigration migrateAll
  entries <- selectList [] [Desc PasswordId]
  liftIO $ print entries
