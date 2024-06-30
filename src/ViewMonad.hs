module ViewMonad (Node (..), Tree, Html (..), VirtualDom, mkVirtualDom, buildHtml) where

import Data.IntMap (IntMap, insert)

data Html = Component Html | Fragment [Html] | Element String [Html] | Text String

data Node
  = ComponentNode Int
  | FragmentNode [Int]
  | ElementNode String [Int]
  | TextNode String
  deriving (Show)

type Tree = IntMap Node

data VirtualDom = VirtualDom
  { _nextId :: Int,
    _tree :: Tree
  }
  deriving (Show)

mkVirtualDom :: VirtualDom
mkVirtualDom = VirtualDom 0 mempty

buildHtml :: Html -> VirtualDom -> (Int, VirtualDom)
buildHtml html vdom =
  let i = _nextId vdom
      vdom' = vdom {_nextId = _nextId vdom + 1}
      (node, vdom'') = case html of
        Component content ->
          let (contentId, vdom2) = buildHtml content vdom'
           in (ComponentNode contentId, vdom2)
        Fragment content ->
          let (contentIds, vdom2) = buildChildren content vdom'
           in (FragmentNode contentIds, vdom2)
        Element tag content ->
          let (contentIds, vdom2) = buildChildren content vdom'
           in (ElementNode tag contentIds, vdom2)
        Text s -> (TextNode s, vdom')
   in (i, vdom'' {_tree = insert i node (_tree vdom'')})

buildChildren :: (Foldable t) => t Html -> VirtualDom -> ([Int], VirtualDom)
buildChildren content vdom =
  foldr
    ( \c (idAcc, vdomAcc) ->
        let (i, vdomAcc') = buildHtml c vdomAcc
         in (idAcc ++ [i], vdomAcc')
    )
    ([], vdom)
    content
