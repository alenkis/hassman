{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cli
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text
import           Db

main :: IO ()
main = do
  migrateDb
  getCliCommand >>= \case
    Init -> do
      sk <- getSensitiveData "Secret key: "
      storeSecretKey sk
    List -> listPasswords
    (Options domain username flag) -> do
      getSensitiveData "Secret key: " >>= verifySecret >>= \case
        Missing -> error "Make sure to initialize with --init"
        NonVerified -> error "Secret key incorrect"
        Verified secret -> case flag of
          Create -> storePassword domain username secret
          Copy   -> copyPassword domain username secret
