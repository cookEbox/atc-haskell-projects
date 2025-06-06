{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module General.Elements ( Tag (..)
                        , Attris(..)
                        , el_ 
                        , elAttr_ 
                        , elAttR_
                        ) where

import           Data.Text       (Text, pack, toLower)
import           Reflex.Dom.Core hiding (tag)

data Tag
  = HTML | HEAD | TITLE | BODY | STYLE
  | FORM | SPAN | DIV   | LABEL
  | H1   | H2   | H3    | H4   | H5 | P | UL -- underline
  deriving stock (Show, Eq)

data Attris = Class    { label :: Text }
            | OnSubmit { label :: Text }

instance Show Attris where 
  show (Class _) = "class" 
  show (OnSubmit _) = "onsubmit" 

showt :: Show a => a -> Text
showt = toLower . pack <$> show

el_ :: forall t m a. DomBuilder t m
    => Tag -> m a -> m a
el_ tag = el (showt tag)

elAttr_ :: forall t m a. DomBuilder t m
        => Tag -> Attris -> m a -> m a
elAttr_ tag attr = elAttr (showt tag) (showt attr =: label attr)

elAttR_ :: forall t m a. DomBuilder t m
        => Tag -> Attris -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elAttR_ tag attr = elAttr' (showt tag) (showt attr =: label attr)
