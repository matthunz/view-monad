{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Test.Hspec
import ViewMonad

app :: Html
app = component_ $ do
  (x, setX) <- useState (0 :: Int)

  return $
    div_
      []
      [ text_ $ show x,
        button_ [on_ "click" $ setX (x + 1)] [text_ "Clone repo!"]
      ]

main :: IO ()
main = hspec $ do
  describe "Data.VirtualDom.rebuildHtml" $ do
    it "rebuilds a single text node" $ do
      let (_, vdom) = buildHtml app mkVirtualDom
          button = fromMaybe (error "TODO") $ find "button" (root vdom)
          (mutations, _) = rebuildHtml 0 (click button)
      mutations `shouldBe` [SetText 4 "1"]
