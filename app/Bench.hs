module Main where

import Criterion.Main
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

run :: VirtualDom -> VirtualDom
run vdom =
  let (_, vdom') = buildHtml app vdom
   in loop 10000000000 vdom'

loop :: Int -> VirtualDom -> VirtualDom
loop 0 v = v
loop n vdom = do
  let vdom' = handle 4 "onclick" vdom
      (mutations, vdom'') = rebuildHtml 0 vdom'
  loop (n - 1) vdom''

main =
  defaultMain
    [ bgroup
        "run"
        [  bench "5" $ whnf run mkVirtualDom
        ]
    ]
