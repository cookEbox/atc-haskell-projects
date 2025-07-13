{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.ProfileShared.Parser where

import           Control.Monad          (void)
import           Data.Either.Extra      (eitherToMaybe)
import           Data.Functor.Identity  (Identity)
import           Data.Text              (Text, pack, unpack)
import           Data.Time.Calendar     (Day, showGregorian, fromGregorian)
import           Reflex.Dom.Core        hiding (count, el)
import           Text.Parsec            (ParseError, ParsecT, count, parse)
import           Text.Parsec.Char       (char, digit, spaces)

type Parse a = ParsecT String () Identity a

gregorianParser :: Parse String
gregorianParser = do
  year <- count 4 digit
  void $ char '-'
  month <- count 2 digit
  void $ char '-'
  day <- count 2 digit
  pure $ concat [day, "/", month, "/", year]

printDate :: Day -> Maybe Text
printDate = fmap pack . eitherToMaybe . parse gregorianParser "DOB" . showGregorian

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

stringDateParser :: String -> Either ParseError Day
stringDateParser = parse dayParser "DOB"

dateParser :: Text -> Maybe Day
dateParser = eitherToMaybe . stringDateParser . unpack
