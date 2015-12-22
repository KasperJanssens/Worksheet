{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Postgres where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Database.Persist.Postgresql

import Data.Time.Clock
import Data.Time.Calendar


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
Work
    day Day
    from UTCTime
    to UTCTime
|]

draai :: IO ()
draai =
  let connStr = "host=localhost dbname=kasper user=kasper password=kasper port=5432" in
   runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $
      flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        utcTime <- liftIO getCurrentTime
        let today = utctDay utcTime
        timeId <- insert $ Work today utcTime utcTime

        michaelId <- insert $ Person "Michael" 26
        michael <- get michaelId
        liftIO $ print michael

