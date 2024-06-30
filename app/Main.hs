module Main where

import ViewMonad

app :: Html
app = component_ $ do
  (x, setX) <- useState (0 :: Int)

  return $ element_ "div" [on_ "click" $ setX (x + 1)] [text_ (show x)]

main :: IO ()
main = do
  let (_, vdom) = buildHtml app mkVirtualDom
  print vdom

  let vdom' = handle 1 "onclick" vdom
      mutations = rebuildHtml 0 vdom'
  print mutations
