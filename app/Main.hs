{-# LANGUAGE LambdaCase #-}
module Main where

import           Cli
import           Db
import           Lib
import           Options.Applicative (execParser)

main :: IO ()
main =
  getCliCommand >>= \case
    (Options domain username flag) -> case flag of
      Create -> createPassword domain username
      Copy -> putStrLn $ "copy password for domain " ++ domain ++ " and username " ++ username
    List -> putStrLn "list all passwords"
