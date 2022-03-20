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

import           Control.Exception       (bracket_)
import           Control.Monad           (liftM)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Crypto.PasswordStore    as PS
import           Data.Elocrypt           (genCapitals, genDigits, genOptions,
                                          genPassword, genSpecials)
import           Data.Maybe
import           Data.Text
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           Data.Time
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           System.Hclip
import           System.IO               (hFlush, hGetEcho, hSetEcho, stdin,
                                          stdout)
import           System.Random           (RandomGen, StdGen, getStdGen)

share
  [ mkMigrate "migrateAll",
    mkPersist sqlSettings
  ]
  [persistLowerCase|
Password
    domain Text
    Domain domain
    username Text
    password Text
    deriving Show
User
    masterPassword Text
    created UTCTime default=CURRENT_TIME
|]

dbName :: Text
dbName = "db:memory"

migrateDb :: IO ()
migrateDb = runSqlite dbName $ do
  runMigration migrateAll
  pure ()

-- | Generates a random password with length `lng` and containing capitals, digits
-- and special characters.
generatePassword :: MonadIO m => Int -> m Text
generatePassword lng =
  pack
    . fst
    . genPassword lng (genOptions {genCapitals = True, genDigits = True, genSpecials = True})
    <$> getStdGen

-- | Stores a hashed password of predefined length for a given `domain` and `username`.
storePassword :: Text -> Text -> IO ()
storePassword domain username = runSqlite dbName $ do
  generatedPass <- generatePassword 12
  hashedPass <- liftIO $ PS.makePassword (encodeUtf8 generatedPass) 14
  passId <- insert $ Password domain username (decodeUtf8 hashedPass)
  password <- get passId
  case password of
    Nothing -> error "Something went wrong"
    Just _ -> liftIO $ print $ "Successfully added password for domain: " ++ unpack domain

-- | Prompts for sensitive text and hides any input.
getSensitiveData :: Text -> IO Text
getSensitiveData title = do
  putStr (unpack title)
  hFlush stdout
  result <- withoutEcho (pack <$> getLine)
  putChar '\n'
  return result
  where
    withoutEcho :: IO a -> IO a
    withoutEcho action = do
      old <- hGetEcho stdin
      bracket_ (hSetEcho stdin False) (hSetEcho stdin old) action

data Verify = Verified Text | NonVerified | Missing
  deriving (Show)

-- | Verifies text input against stored hashed user password.
verifyMasterPassword :: Text -> IO Verify
verifyMasterPassword input = runSqlite dbName $ do
  masterP <- listToMaybe <$> selectList [] [Desc UserCreated]
  liftIO $ case masterP of
    Nothing -> pure Missing
    Just p -> do
      let passwordValue = (encodeUtf8 . userMasterPassword . entityVal) p
      if PS.verifyPassword (encodeUtf8 input) passwordValue
        then pure (Verified input)
        else pure NonVerified

-- | Stores a hashed master password.
storeMasterPassword :: Text -> IO ()
storeMasterPassword password = runSqlite dbName $ do
  time <- liftIO getCurrentTime
  hashedPassword <- decodeUtf8 <$> liftIO (PS.makePassword (encodeUtf8 password) 14)
  insert $ User hashedPassword time
  liftIO $ print "Successfully stored master password."

-- | Try to retrieve a password for `domain`/`username` combination and if successful,
-- copy password to clipoard.
copyPassword :: Text -> Text -> IO ()
copyPassword domain username = runSqlite dbName $ do
  password <- getBy $ Domain domain
  liftIO $ case password of
    Nothing -> print "Could not find password."
    Just p -> do
      setClipboard (unpack $ passwordPassword $ entityVal p)
      print "Copied password to clipboard."

listPasswords :: IO ()
listPasswords = runSqlite dbName $ do
  entries <- selectList [] [Desc PasswordId]
  liftIO $ print entries
