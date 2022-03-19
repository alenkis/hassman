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

data Command = Create | Copy
  deriving (Eq, Show)

data Input
  = Options Domain Username Command
  | List
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

commandFlagP :: Parser Command
commandFlagP = flag Copy Create (short 'c' <> long "create")

optionsP :: Parser Input
optionsP = Options <$> domainP <*> usernameP <*> commandFlagP

listPasswordsP :: Parser Input
listPasswordsP = flag' List (short 'l' <> long "list" <> help "List all passwords")

input :: Parser Input
input = optionsP <|> listPasswordsP

getCliCommand :: IO Input
getCliCommand =
  execParser
    ( info
        (helper <*> versionOption <*> input)
        (header version <> fullDesc)
    )
  where
    versionOption = infoOption version (long "version" <> short 'v' <> help "Show version")
    version = "0.0.1"
