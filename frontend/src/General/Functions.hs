{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module General.Functions where

import           Common.Api
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (ToJSON)
import           Data.Map.Strict             (singleton, (!))
import           Data.Text                   (Text)
import           Language.Javascript.JSaddle (MonadJSM, liftJSM)
import           Reflex.Dom.Core

data AppState t = AppState
  { appLoggedIn    :: Dynamic t (Maybe UserInfo)
  , refreshUserReq :: () -> IO ()
  , profileUidDyn  :: Dynamic t (Maybe Integer)
  , profileClick   :: Integer -> IO ()
  } 

flattenDyn :: Reflex t => Dynamic t (Dynamic t a) -> Dynamic t a
flattenDyn dd =
  (\mp -> mp ! ()) <$> joinDynThroughMap (singleton () <$> dd)

sendRequest :: ( MonadJSM (Performable m)
               , PerformEvent t m
               , TriggerEvent t m
               , ToJSON a
               ) => Text -> Event t a -> m (Event t XhrResponse)
sendRequest path event = performRequestAsync 
                       $ postJson ("http://localhost:8000/" <> path) <$> event

tagger :: Reflex t
       => Dynamic t Text
       -> Dynamic t Text
       -> Event t a
       -> Event t UserDetailsReq
tagger usernameDyn hashedPasswordDyn loginEv =
  tag (current $ UserDetailsReq <$> usernameDyn <*> hashedPasswordDyn) loginEv

updateState :: (PerformEvent t1 m, MonadJSM (Performable m))
            => AppState t2 -> Event t1 a -> m ()
updateState appState success = do
    performEvent_ $ ffor success $ \_ -> liftJSM $ do
      liftIO $ refreshUserReq appState ()

