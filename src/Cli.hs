{-# LANGUAGE LambdaCase #-}

module Cli where

import           Options.Applicative         (Parser, command, execParser, flag,
                                              fullDesc, header, help, helper,
                                              info, infoOption, long, metavar,
                                              short, strOption, subparser,
                                              (<|>))
import           Options.Applicative.Builder (flag')

type Domain = String

type Username = String

data Command = Create | List
  deriving (Eq, Show)

data Options = Options Domain Username (Maybe Command)
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

flagP :: Parser (Maybe Command)
flagP =
  flag Nothing (Just Create) (short 'c' <> long "create")
    <|> flag Nothing (Just List) (short 'l' <> long "list")

parseOpts :: Parser Options
parseOpts = Options <$> domainP <*> usernameP <*> flagP

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
