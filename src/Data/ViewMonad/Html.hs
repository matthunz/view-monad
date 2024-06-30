-- |
-- Copyright   :  (c) Matt Hunzinger 2024
-- License     :  BSD-3
-- Maintainer  : matthunz2@gmail.com
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

import Data.ViewMonad

data HtmlAttributeValue = TextValue String | Handler (Scope ())

instance Show HtmlAttributeValue where
  show (TextValue s) = s
  show (Handler _) = "<<Handler>>"

data HtmlAttribute = HtmlAttribute String HtmlAttributeValue
  deriving (Show)

on_ :: String -> Scope () -> HtmlAttribute
on_ n s = HtmlAttribute ("on" ++ n) (Handler s)

data Html = HtmlComponent !(Component Html) | Fragment ![Html] | Element !String ![HtmlAttribute] ![Html] | Text !String
  deriving (Show)

component_ :: Component Html -> Html
component_ = HtmlComponent

element_ :: String -> [HtmlAttribute] -> [Html] -> Html
element_ = Element

div_ :: [HtmlAttribute] -> [Html] -> Html
div_ = element_ "div"

button_ :: [HtmlAttribute] -> [Html] -> Html
button_ = element_ "button"

text_ :: String -> Html
text_ = Text
