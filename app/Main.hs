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
      mp <- getSensitiveData "Masterpass: "
      storeMasterPassword mp
    List -> listPasswords
    (Options domain username flag) -> do
      masterPassword <- getSensitiveData "Password: "
      isVerified <- verifyMasterPassword masterPassword
      case isVerified of
        Missing -> error "Make sure to initialize with --init"
        NonVerified -> error "Master password incorrect"
        Verified _ -> case flag of
          Create -> storePassword domain username
          Copy   -> copyPassword domain username
