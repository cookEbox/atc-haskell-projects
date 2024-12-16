{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Types where

import           Control.Monad                        (void, when)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Reader           (ReaderT)
import           Data.Char                            (toLower)
import           Data.Function                        (on)
import           Data.Functor.Identity                (Identity)
import           Data.Kind                            (Type)
import           Data.List                            (sortBy, transpose)
import           Data.Maybe                           (fromJust, fromMaybe,
                                                       isNothing)
import           Data.String                          (IsString, fromString)
import           Data.Text                            (Text, pack, unpack)
import           Data.Time                            (Day)
import           Data.Time.Format                     (defaultTimeLocale,
                                                       parseTimeM)
import           Database.Persist                     hiding (Add, count)
import           Database.Persist.Sql                 (PersistField,
                                                       PersistFieldSql,
                                                       SqlType (SqlString),
                                                       fromSqlKey, runMigration,
                                                       sqlType, toSqlKey)
import           Database.Persist.SqlBackend.Internal (SqlBackend)
import           Database.Persist.Sqlite              (runSqlite)
import           Database.Persist.TH
import           GHC.Generics                         (Generic)
import           GHC.Int                              (Int64)
import           System.Console.Haskeline
import           System.IO                            (hFlush, stdout)
import           Text.Parsec

data Status = Complete
            | InProgress
            | ToDo
            deriving (Show, Eq, Read, Generic)

data Level = High
           | Medium
           | Low
           deriving (Show, Eq, Read, Generic, Ord)

instance IsString Level where
  fromString "High"   = High
  fromString "Medium" = Medium
  fromString "Low"    = Low
  fromString "high"   = High
  fromString "medium" = Medium
  fromString "low"    = Low

instance IsString Status where
  fromString "Complete"   = Complete
  fromString "InProgress" = InProgress
  fromString "ToDo"       = ToDo

derivePersistField "Status"

derivePersistField "Level"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Task
    description String
    status      Status
    dueDate     (Maybe Day)
    priority    Level
    deriving Show Eq
|]

type Id = Int64

type Parse a = ParsecT String () Identity a

type Action m = ReaderT SqlBackend m ()

data Group = ID Id
           | All
           | STS Status
           | PRI
           | Due
           deriving (Show, Eq)

data Commands = Add (Maybe Day)
              | View Group
              | Mark Id Status
              | Delete Id
              | Help
              | Priority Id Level
              | Edit Id (Maybe String)
              | Date Id (Maybe Day)
              deriving (Show, Eq)

