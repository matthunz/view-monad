module Main where

import ViewMonad

main :: IO ()
main =
  let (_, vdom) = buildHtml (Fragment [Component $ Text "A", Text "B"]) mkVirtualDom
   in print vdom
