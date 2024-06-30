module Main where

import ViewMonad

main :: IO ()
main =
  let (_, vdom) = buildHtml (HtmlComponent . pure $ Text "A") mkVirtualDom
      mutations = rebuildHtml 0 vdom
   in print mutations
