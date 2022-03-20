{-# LANGUAGE LambdaCase #-}
module Main where

import           Cli
import           Db

main :: IO ()
main =
  getCliCommand >>= \case
    (Options domain username flag) -> case flag of
      Create -> createPassword domain username "secret"
      Copy   -> getPassword domain username
    List -> listPasswords
