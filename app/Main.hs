{-# LANGUAGE LambdaCase #-}

module Main where

import           Cli
import           Lib
import           Options.Applicative (execParser)

main :: IO ()
main =
  getCliCommand >>= \case
    (Options domain username flag) -> case flag of
      Create -> putStrLn $ "create password for domain " ++ domain ++ " and username " ++ username
      Copy -> putStrLn $ "copy password for domain " ++ domain ++ " and username " ++ username
    List -> putStrLn "list all passwords"
