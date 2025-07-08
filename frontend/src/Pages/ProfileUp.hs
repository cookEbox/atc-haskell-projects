{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.ProfileUp where

import           Common.Api
import           Common.Route
import           Control.Monad          (void)
import           Data.Aeson             (ToJSON)
import           Data.Either.Extra      (eitherToMaybe)
import           Data.Functor.Identity  (Identity)
import           Data.Maybe             (fromMaybe)
import           Data.Text              as T (Text, isInfixOf, null, strip, unpack)
import           Data.Time.Calendar     (Day, fromGregorian)
import           General.Buttons
import           General.Elements
import           General.Functions
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Prelude                hiding (null)
import           Reflex.Dom.Core        hiding (count, el, elAttr, elAttr')
import           Text.Parsec            (ParsecT, ParseError, count, parse)
import           Text.Parsec.Char       (char, digit, spaces)

type Parse a = ParsecT String () Identity a

fieldToDyn :: Reflex t => InputElement EventResult s t -> Dynamic t (Maybe Text)
fieldToDyn el  = dynMbTxts <$> _inputElement_value el
  where
    dynMbTxts txt = if null (strip txt)
                    then Nothing
                    else Just txt

parsedFieldToDyn :: Reflex t => InputElement EventResult s t -> Dynamic t (Maybe Day)
parsedFieldToDyn el = dateParser <$> _inputElement_value el

dayParser :: Parse Day
dayParser = do
  spaces
  day <- count 2 digit
  void $ char '/'
  month <- count 2 digit
  void $ char '/'
  year <- count 4 digit
  pure $ fromGregorian (read year) (read month) (read day)

gregorianParser :: String -> Either ParseError Day
gregorianParser = parse dayParser "DOB"

dateParser :: Text -> Maybe Day
dateParser = eitherToMaybe . gregorianParser . unpack

profileUpdate :: ( ToJSON a, SetRoute t (R FrontendRoute) (Client m)
                 , Monad m
                 , Prerender t m
                 ) => Event t a -> m (Dynamic t Text)
profileUpdate updateDataEv = do
  nestedDyn <- prerender (pure $ constDyn "") $ do
    resp <- sendRequest "sprofileup" updateDataEv
    let txtEv   = fmap (fromMaybe "" . _xhrResponse_responseText) resp
        success = ffilter ("Success" `T.isInfixOf`) txtEv
        failure = ffilter (not . ("Success" `T.isInfixOf`)) txtEv
    setRoute ((FrontendRoute_Profile :/ ()) <$ success)
    holdDyn "" failure
  pure $ flattenDyn nestedDyn

profileSubmit :: ObeliskWidget t (R FrontendRoute) m
              => AppState t -> RoutedT t () m ()
profileSubmit appState = do
  elClass_ DIV "profile-page" $ do
    elClass_ DIV "profile-form" $ mdo
      -- TODO: Update the buttons for better user experience
      loginControlButton LoginAndMain appState
      el_ H1 $ text "UPDATE PROFILE PAGE"
      (formEl, _) <- elAttR_ FORM (single $ OnSubmit "return false;") $ do
        rec
          usernameEl <- elClass_ DIV "field-group" $ do
            el_ LABEL $ text "Username: "
            textBox NotPassword clearEv Persistent

          dobEl <- elClass_ DIV "field-group" $ do
            el_ LABEL $ text "DOB (dd/mm/yyyy): "
            textBox NotPassword clearEv Persistent

          locationEl <- elClass_ DIV "field-group" $ do
            el_ LABEL $ text "City/County: "
            textBox NotPassword clearEv Persistent

          hobbiesEl <- elClass_ DIV "field-group" $ do
            el_ LABEL $ text "Hobbies: "
            textBox NotPassword clearEv Persistent

          bioEl <- elClass_ DIV "field-group" $ do
            el_ LABEL $ text "Bio: "
            textBox NotPassword clearEv Persistent

          elAttr_ BUTTON (multi [Type "submit", Class "btn"]) $ text "Update"

          let submitEv     = domEvent Submit formEl
              uidDyn       = fmap uiId <$> appLoggedIn appState
              clearEv      = "" <$ submitEv
              profileDyn   = UserProfile <$> fieldToDyn usernameEl
                                         <*> parsedFieldToDyn dobEl
                                         <*> fieldToDyn locationEl
                                         <*> fieldToDyn hobbiesEl
                                         <*> fieldToDyn bioEl
                                         <*> uidDyn
              updateDataEv = tag (current profileDyn) submitEv

          failureDyn <- profileUpdate updateDataEv
        el_ DIV $ dynText failureDyn
        pure ()
      pure ()
