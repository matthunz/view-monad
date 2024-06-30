module Main where

import ViewMonad

main :: IO ()
main =
  let (_, vdom) =
        buildHtml
          ( HtmlComponent $ do
              (x, setX) <- useState (0 :: Int)

              setX (x + 1)

              return $ Text (show x)
          )
          mkVirtualDom
      mutations = rebuildHtml 0 vdom
   in print mutations
