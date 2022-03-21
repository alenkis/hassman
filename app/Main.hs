{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cli                    (Command (Copy, Create),
                                         Input (Init, List, Options),
                                         getCliCommand)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              ()
import           Db                     (SecretState (Missing, NonVerified, Verified),
                                         copyPassword, getSensitiveData,
                                         listPasswords, migrateDb,
                                         storePassword, storeSecretKey,
                                         verifySecret)
import qualified Theme

main :: IO ()
main = do
  migrateDb
  getCliCommand >>= \case
    Init -> do
      sk <- getSensitiveData $ Theme.info "Secret key: "
      storeSecretKey sk
    List -> listPasswords
    (Options domain username flag) -> do
      getSensitiveData "Secret key: " >>= verifySecret >>= \case
        Missing -> putStrLn (Theme.danger "Make sure to initialize with" ++ Theme.info " --init")
        NonVerified -> putStrLn $ Theme.danger "Secret key incorrect"
        Verified secret -> case flag of
          Create -> storePassword domain username secret
          Copy   -> copyPassword domain username secret
