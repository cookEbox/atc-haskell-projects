{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module General.Elements ( Tag (..)
                        , Attris(..)
                        , el_
                        , elR_
                        , elAttr_
                        , elAttR_
                        , AttrisList
                        , single
                        , multi
                        , elClass_
                        ) where

import Control.Lens.At (At, Index, IxValue)
import           Data.List.NonEmpty      (NonEmpty, fromList)
import           Data.Semigroup.Foldable (foldMap1)
import           Data.Text               (Text, pack, toLower)
import           Reflex.Dom.Core         hiding (tag)

data Tag
  = HTML | HEAD | TITLE | BODY | STYLE
  | FORM | SPAN | DIV   | LABEL | BUTTON
  | H1   | H2   | H3    | H4   | H5 | P | UL | LI | I | A
  deriving stock (Show, Eq)

data Attris = Class    { label :: Text }
            | OnSubmit { label :: Text }
            | Type     { label :: Text }
            | Href     { label :: Text }

instance Show Attris where
  show (Class _)    = "class"
  show (Type  _)    = "type"
  show (OnSubmit _) = "onsubmit"
  show (Href _)     = "href"

type AttrisList = NonEmpty Attris
single :: Attris -> AttrisList
single = fromList . (:[])

multi :: [Attris] -> AttrisList
multi = fromList

showt :: Show a => a -> Text
showt = toLower . pack <$> show

el_ :: forall t m a. DomBuilder t m
    => Tag -> m a -> m a
el_ tag = el (showt tag)

elR_ :: forall t m a. DomBuilder t m
    => Tag -> m a -> m (Element EventResult (DomBuilderSpace m) t, a) 
elR_ tag = el' (showt tag)

shobel :: ( At m
          , Monoid m
          , Index m ~ Text
          , IxValue m ~ Text
          ) => AttrisList -> m      
shobel attrs = foldMap1 (\attr -> (showt attr =: label attr)) attrs

elAttr_ :: forall t m a. DomBuilder t m
        => Tag -> AttrisList -> m a -> m a
elAttr_ tag attrs = elAttr (showt tag) (shobel attrs)

elAttR_ :: forall t m a. DomBuilder t m
        => Tag
        -> AttrisList
        -> m a
        -> m (Element EventResult (DomBuilderSpace m) t, a)
elAttR_ tag attrs = elAttr' (showt tag) (shobel attrs)

elClass_ :: DomBuilder t m => Tag -> Text -> m a -> m a
elClass_ tag lbl = elAttr_ tag (single $ Class lbl)
