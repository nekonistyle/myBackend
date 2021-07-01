-- general
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
-- servant
{-# LANGUAGE DataKinds                  #-}
-- persistent
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}


module Backend
    ( DatabaseIO
    , readyPool
    , runServer
    , runServerWithCors
    , dbAction
    ) where

-- general
import Control.Monad.Reader
import Control.Monad.Logger
import Data.String.Conversions
-- servant
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
-- persistent
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
-- CORS
import Network.Wai.Middleware.Cors


type DatabaseIO a = ReaderT SqlBackend IO a

readyPool :: FilePath -> Migration -> IO ConnectionPool
readyPool dbPath migration = do
  pool <- mkPool dbPath
  runSqlPool (runMigration migration) pool
  return pool

runServer :: HasServer api '[] => Int -> Proxy api -> (ConnectionPool -> Server api) -> ConnectionPool -> IO ()
runServer port proxy server pool =
  run port $ serve proxy $ server pool

dbAction :: ConnectionPool -> DatabaseIO a -> Handler a
dbAction pool action =
  liftIO $ runSqlPool action pool


--ConnectionPool

poolSize :: Int
poolSize = 5

mkPool :: FilePath -> IO ConnectionPool
mkPool filePath =
  runStdoutLoggingT $ createSqlitePool (cs filePath) poolSize

--CORS
runServerWithCors :: HasServer api '[] => Int -> Proxy api -> (ConnectionPool -> Server api) -> ConnectionPool -> IO ()
runServerWithCors port proxy server pool =
  run port $ cors (const $ Just policy) $ serve proxy $ server pool
  where
    policy :: CorsResourcePolicy
    policy = simpleCorsResourcePolicy
             { corsRequestHeaders = [ "content-type" ] }

