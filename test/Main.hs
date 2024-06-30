{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
    it "returns the first element of a list" $ do
      let (_, vdom) = buildHtml app mkVirtualDom
          vdom' = handle 2 "onclick" vdom
          (mutations, _) = rebuildHtml 0 vdom'
      mutations `shouldBe` [SetText 4 "1"]
