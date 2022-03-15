{-# LANGUAGE LambdaCase #-}

module Cli where

import           Options.Applicative (Parser, execParser, fullDesc, header,
                                      help, helper, info, infoOption, long,
                                      metavar, short, strOption)

type Domain = String

type Username = String

data Options = Options Domain Username
  deriving (Eq, Show)

domainP :: Parser Domain
domainP =
  strOption $
    short 'd'
      <> long "domain"
      <> metavar "DOMAIN"
      <> help "Password domain"

usernameP :: Parser Username
usernameP =
  strOption $
    short 'u'
      <> long "username"
      <> metavar "USERNAME"
      <> help "Password username"

parseOpts :: Parser Options
parseOpts = Options <$> domainP <*> usernameP

getCliCommand :: IO Options
getCliCommand =
  execParser
    ( info
        (helper <*> versionOption <*> parseOpts)
        (header version <> fullDesc)
    )
  where
    versionOption = infoOption version (long "version" <> short 'v' <> help "Show version")
    version = "0.0.1"
