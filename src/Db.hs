{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- In order to get a version of HLS that supports TemplateHaskell pragma,
-- steps outline here need to followed: https://github.com/haskell/haskell-language-server/issues/2659#issuecomment-1048984537

-- |
module Db where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Text
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share
  [ mkMigrate "migrateAll",
    mkPersist sqlSettings
  ]
  [persistLowerCase|
Password
    url String
    username String
    deriving Show
|]

createPassword :: String -> String -> IO ()
createPassword url username = runSqlite "db:memory" $ do
  runMigration migrateAll
  pId <- insert $ Password url username
  p <- get pId
  liftIO $ print $ "successfully added password for domain: " ++ url
