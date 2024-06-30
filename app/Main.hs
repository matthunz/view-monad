module Main where

import Data.Maybe (fromMaybe)
import ViewMonad

app :: Html IO
app = component_ $ do
  (x, setX) <- useState (0 :: Int)

  return $
    div_
      []
      [ text_ $ "Useful files: " ++ show x,
        button_ [on_ "click" $ setX (x + 1)] [text_ "Clone repo!"],
        button_ [on_ "click" $ setX (x - 1)] [text_ "Download meme!"]
      ]

main :: IO ()
main = do
  (_, vdom) <- buildHtml app mkVirtualDom
  print vdom

  let button = fromMaybe (error "TODO") $ find "button" (root vdom)
  (mutations, vdom') <- rebuildHtml 0 (click button)
  print mutations

  let button' = fromMaybe (error "TODO") $ find "button" (root vdom')
  (mutations2, _) <- rebuildHtml 0 (click button')
  print mutations2
