module Main where

import ViewMonad

app :: Html
app = component_ $ do
  (x, setX) <- useState (0 :: Int)

  setX (x + 1)

  return $ element_ "div" [text_ (show x)]

main :: IO ()
main =
  let (_, vdom) = buildHtml app mkVirtualDom
      mutations = rebuildHtml 0 vdom
   in print mutations
