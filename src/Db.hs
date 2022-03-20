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
import           System.Hclip

share
  [ mkMigrate "migrateAll",
    mkPersist sqlSettings
  ]
  [persistLowerCase|
Password
    domain String
    Domain domain
    username String
    password String
    deriving Show
|]

dbName :: Text
dbName = "db:memory"

migrateDb :: IO ()
migrateDb = runSqlite dbName $ do
  runMigration migrateAll
  pure ()

-- TODO: hash password
createPassword :: String -> String -> String -> IO ()
createPassword domain username password = runSqlite dbName $ do
  pId <- insert $ Password domain username password
  p <- get pId
  case p of
    Nothing -> error "Something went wrong"
    Just _ -> liftIO $ print $ "Successfully added password for domain: " ++ domain

getPassword :: String -> String -> IO ()
getPassword domain username = runSqlite dbName $ do
  password <- getBy $ Domain domain
  liftIO $ case password of
    Nothing -> print "Could not find password."
    Just p -> do
      setClipboard (passwordPassword $ entityVal p)
      print "Copied password to clipboard."

listPasswords :: IO ()
listPasswords = runSqlite dbName $ do
  entries <- selectList [] [Desc PasswordId]
  liftIO $ print entries
