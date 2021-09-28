{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib
    ( startApp
    , generate
    ) where

import Control.Monad.Reader
import Data.Aeson
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Elm
import Elm.Derive as ED
import Servant
import Servant.Elm

import Backend
import ElmGenerator


port :: Int
port = 8080

dbPath :: FilePath
dbPath = "sqlite.db"


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Memo
  title   String
  content String
|]

deriveBoth ED.defaultOptions ''Memo


type API = "allMemos" :> Get '[JSON] [Memo]
  :<|> "postMemo" :> ReqBody '[JSON] Memo :> Post '[JSON] Memo

server :: Server API
server = getAll
  :<|> insertMemo


getAll :: MonadIO m => m [Memo]
getAll = runDB dbGetAll

insertMemo :: MonadIO m => Memo -> m Memo
insertMemo = runDB . dbInsertMemo


dbGetAll :: DatabaseIO [Memo]
dbGetAll = do
  ((<$>) entityVal) <$> selectList [] []

dbInsertMemo :: Memo -> DatabaseIO Memo
dbInsertMemo memo = do
  insert memo >> return memo


startApp :: IO ()
startApp =
  runServerWithCors port (Proxy :: Proxy API) server

generate :: IO ()
generate =
  generateElm port deList (Proxy :: Proxy API) "Api"
  where
    deList = [DefineElm (Proxy :: Proxy Memo)]

-- runDB
runDB :: MonadIO m => DatabaseIO a -> m a
runDB = runDatabaseIO dbPath migrateAll
