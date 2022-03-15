{-# LANGUAGE LambdaCase #-}

module Main where

import           Cli
import           Lib
import           Options.Applicative (execParser)

main :: IO ()
main =
  getCliCommand >>= \case
    (Options d u) -> putStrLn $ "\ndomain: " ++ d ++ "\nusername: " ++ u
