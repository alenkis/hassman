{-# LANGUAGE LambdaCase #-}

module Main where

import           Cli
import           Lib
import           Options.Applicative (execParser)

main :: IO ()
main =
  getCliCommand >>= \case
    (Options domain username flag) -> case flag of
      Just Create -> putStrLn $ "create password for domain " ++ domain ++ " and username " ++ username
      Just List -> putStrLn $ "list password for domain " ++ domain ++ " and username " ++ username
      Nothing -> putStrLn $ "copy password for domain" ++ domain ++ " and username " ++ username
