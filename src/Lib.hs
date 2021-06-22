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

module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Servant
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics

import Backend


port :: Int
port = 8080

dbPath :: FilePath
dbPath = "sqlite.db"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  userFirstName  String
  userLastName   String
  deriving Generic
|]

-- $(deriveJSON defaultOptions ''User)
instance FromJSON User
instance ToJSON User

type API = "getAllUsers" :> Get '[JSON] [User]
           :<|> "postUser" :> ReqBody '[JSON] User :> Post '[JSON] User

server :: ConnectionPool -> Server API
server pool = do
  dbAction pool getAll :<|> dbAction pool . insertUser

getAll :: DatabaseIO [User]
getAll = do
  allUsers <- selectList [] []
  return $ map entityVal allUsers

insertUser :: User -> DatabaseIO User
insertUser user = do
  userId <- insert user
  return user

startApp :: IO ()
startApp = do
  pool <- readyPool dbPath migrateAll
  runServer port (Proxy :: Proxy API) server pool


