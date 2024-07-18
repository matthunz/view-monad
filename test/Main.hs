{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright   :  (c) Matt Hunzinger 2024
-- License     :  BSD-3
-- Maintainer  :  matthunz2@gmail.com
module Main (main) where

import Control.Lens
import Data.Maybe (fromMaybe)
import Test.Hspec
import ViewMonad

data Counter = Counter
  { _counter :: Int
  }

makeLenses ''Counter

app :: (Monad m) => Html m
app = component_ (Counter 0) $ do
  (count, setCount) <- useState counter

  return $
    div_
      []
      [ text_ $ show count,
        button_ [on_ "click" $ setCount (count + 1)] [text_ "Clone repo!"],
        button_ [on_ "click" $ setCount (count - 1)] [text_ "Download meme!"]
      ]

main :: IO ()
main = hspec $ do
  describe "Data.VirtualDom.rebuildHtml" $ do
    it "rebuilds a single text node" $ do
      (_, vdom) <- buildHtml app mkVirtualDom
      let button = fromMaybe (error "TODO") $ find "button" (root vdom)
      (mutations, _) <- rebuildHtml 0 (click button)
      mutations `shouldBe` [SetText 2 "1"]
