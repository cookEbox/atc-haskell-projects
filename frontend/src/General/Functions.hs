{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}

module General.Functions where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Text                   (Text, isInfixOf, splitOn, stripPrefix, strip)
import           Data.Time.Clock             (getCurrentTime)
import           Language.Javascript.JSaddle (JSM, eval, liftJSM,
                                              strToText, valToStr)
import           Reflex.Dom.Core

getCookies :: JSM Text
getCookies = strToText <$> (valToStr =<< eval ("document.cookie" :: Text))

cookieWatcher :: (MonadWidget t m) => m (Dynamic t Text)
cookieWatcher = do
  tick <- tickLossy 1 =<< liftIO getCurrentTime
  cookieEvent <- performEvent (liftJSM getCookies <$ tick)
  holdDyn "" cookieEvent

statusCookie :: Functor f => f Text -> f Bool
statusCookie cookieDyn = isInfixOf "status=loggedIn" <$> cookieDyn

parseCookie :: Text -> Maybe (Text, Text) 
parseCookie cookieText = 
  case (authEntry, nameEntry) of 
    ((entryA:_),(entryN:_)) -> (,) <$> (stripPrefix "auth=" (strip entryA)) <*> (stripPrefix "user=" (strip entryN))
    _                       -> Nothing
  where 
    cookies = splitOn ";" cookieText 
    authEntry = filter ((isInfixOf "auth=") . strip) cookies
    nameEntry = filter ((isInfixOf "user=") . strip) cookies
