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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Twits
    name       Text
    password   Text
    followers  [TwitsId]
    following  [TwitsId]
    UniqueTwit name
    updated_at UTCTime
    deriving Show Eq
Tweets
    user_name  Text
    user_id    TwitsId
    likes      [TwitsId]
    reply_id   [TweetsId]
    content    Text
    created_at UTCTime
    updated_at UTCTime
    deriving Show Eq
|]

