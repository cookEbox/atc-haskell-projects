{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}

module Database.DB where

import           Data.Text           (Text)
import           Data.Time.Clock     (UTCTime)
import           Database.Persist.TH
import           GHC.Int             (Int64)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Twits
    name       Text
    password   Text
    follow     [Int64]
    UniqueTwit name
    updated_at UTCTime
    deriving Show Eq
Tweets
    user_name  Text
    user_id    Int64
    likes      [Int64]
    reply_id   [Int64]
    content    Text
    created_at UTCTime
    updated_at UTCTime
    deriving Show Eq
|]

