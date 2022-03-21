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
import           Crypto.TripleSec
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as Char8
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
    password ByteString
    deriving Show
User
    secretKey Text
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
generatePassword :: MonadIO m => Int -> m ByteString
generatePassword lng =
  Char8.pack
    . fst
    . genPassword lng (genOptions {genCapitals = True, genDigits = True, genSpecials = True})
    <$> getStdGen

-- | Stores a hashed password of predefined length for a given `domain` and `username`.
storePassword :: Text -> Text -> Text -> IO ()
storePassword domain username secret = runSqlite dbName $ do
  generatedPass <- generatePassword 12
  hashedPass <- liftIO $ encryptIO (encodeUtf8 secret) generatedPass
  passId <- insert $ Password domain username hashedPass
  password <- get passId
  case password of
    Nothing -> error "Something went wrong with storing password."
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

data SecretState = Verified Text | NonVerified | Missing
  deriving (Show)

-- | Verifies text input against latest stored hashed user secret key.
verifySecret :: Text -> IO SecretState
verifySecret input = runSqlite dbName $ do
  -- Get the latest secret key
  secretKey <- listToMaybe <$> selectList [] [Desc UserCreated]
  liftIO $ case secretKey of
    Nothing -> pure Missing
    Just sk -> do
      if isPasswordVerified
        then pure (Verified input)
        else pure NonVerified
      where
        passwordValue = (encodeUtf8 . userSecretKey . entityVal) sk
        isPasswordVerified = PS.verifyPassword (encodeUtf8 input) passwordValue

-- | Stores a hashed secret key.
storeSecretKey :: Text -> IO ()
storeSecretKey secretKey = runSqlite dbName $ do
  time <- liftIO getCurrentTime
  hashedPassword <- liftIO $ PS.makePassword (encodeUtf8 secretKey) 14
  insert $ User (decodeUtf8 hashedPassword) time
  liftIO $ print "Successfully stored secret key."

-- | Try to retrieve a password for `domain`/`username` combination and if successful,
-- copy password to clipoard.
copyPassword :: Text -> Text -> Text -> IO ()
copyPassword domain username secret = runSqlite dbName $ do
  maybePasswordEntity <- getBy $ Domain domain
  liftIO $ case maybePasswordEntity of
    Nothing -> print "Could not find password."
    Just p -> do
      case decrypted of
        Left ex -> print $ "Could not decrypt password: " ++ show ex
        Right password -> do
          -- Copy decrypted password to clipboard
          setClipboard (unpack $ decodeUtf8 password)
          print "Copied password to clipboard."
      where
        passwordValue = passwordPassword $ entityVal p
        decrypted = runTripleSecDecryptM $ decrypt (encodeUtf8 secret) passwordValue

listPasswords :: IO ()
listPasswords = runSqlite dbName $ do
  entries <- selectList [] [Desc PasswordId]
  liftIO $ print entries
