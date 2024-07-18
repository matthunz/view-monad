{-# LANGUAGE GADTs #-}

-- |
-- Copyright   :  (c) Matt Hunzinger 2024
-- License     :  BSD-3
-- Maintainer  :  matthunz2@gmail.com
module Data.ViewMonad.Html
  ( HtmlAttribute (..),
    HtmlAttributeValue (..),
    on_,
    Html (..),
    component_,
    element_,
    text_,
    div_,
    button_,
  )
where

import Data.Typeable
import Data.ViewMonad

data HtmlAttributeValue m = TextValue String | Handler (Scope m ())

instance Show (HtmlAttributeValue m) where
  show (TextValue s) = s
  show (Handler _) = "<<Handler>>"

data HtmlAttribute m = HtmlAttribute String (HtmlAttributeValue m)
  deriving (Show)

on_ :: String -> Scope m () -> HtmlAttribute m
on_ n s = HtmlAttribute ("on" ++ n) (Handler s)

data Html m
  = HtmlComponent !(DynComponent m (Html m))
  | Fragment ![Html m]
  | Element !String ![HtmlAttribute m] ![Html m]
  | Text !String

component_ :: (Typeable s) => s -> Component m s (Html m) -> Html m
component_ s c = HtmlComponent (DynComponent s c)

element_ :: String -> [HtmlAttribute m] -> [Html m] -> Html m
element_ = Element

div_ :: [HtmlAttribute m] -> [Html m] -> Html m
div_ = element_ "div"

button_ :: [HtmlAttribute m] -> [Html m] -> Html m
button_ = element_ "button"

text_ :: String -> Html m
text_ = Text
