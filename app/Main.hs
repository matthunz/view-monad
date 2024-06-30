module Main where

import ViewMonad

app :: Html
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
  let (_, vdom) = buildHtml app mkVirtualDom
  print vdom

  let vdom' = handle 4 "onclick" vdom
      (mutations, vdom'') = rebuildHtml 0 vdom'
  print mutations

  let vdom''' = handle 4 "onclick" vdom''
      (mutations2, _) = rebuildHtml 0 vdom'''
  print mutations2
