{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cli                    (Command (Copy, Create),
                                         Input (Init, List, Options),
                                         getCliCommand)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              ()
import           Db                     (Verify (Missing, NonVerified, Verified),
                                         copyPassword, getSensitiveData,
                                         listPasswords, migrateDb,
                                         storePassword, storeSecretKey,
                                         verifySecret)

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
