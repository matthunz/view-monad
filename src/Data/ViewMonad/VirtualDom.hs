{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright   :  (c) Matt Hunzinger 2024
-- License     :  BSD-3
-- Maintainer  : matthunz2@gmail.com
module Data.ViewMonad.VirtualDom
  ( Node (..),
    Tree,
    VirtualDom,
    Mutation (..),
    mkVirtualDom,
    buildHtml,
    rebuildHtml,
    handle,
    handle',
    update,
  )
where

import Data.Dynamic (Dynamic)
import Data.Foldable (foldr')
import Data.IntMap (IntMap, adjust, insert, (!))
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.ViewMonad
import Data.ViewMonad.Html

data Node
  = ComponentNode !(Component Html) ![Dynamic] !Int
  | FragmentNode ![Int]
  | ElementNode !String ![HtmlAttribute] ![Int]
  | TextNode !String
  deriving (Show)

type Tree = IntMap Node

data VirtualDom = VirtualDom
  { _nextId :: !Int,
    _tree :: !Tree
  }
  deriving (Show)

mkVirtualDom :: VirtualDom
mkVirtualDom = VirtualDom 0 mempty

buildHtml :: Html -> VirtualDom -> (Int, VirtualDom)
buildHtml html vdom =
  let i = _nextId vdom
      vdom' = vdom {_nextId = _nextId vdom + 1}
      (node, vdom'') = case html of
        HtmlComponent content ->
          let (contentHtml, _, hooks) = runComponent content i 0 []
              (contentId, vdom2) = buildHtml contentHtml vdom'
           in (ComponentNode content hooks contentId, vdom2)
        Fragment content ->
          let (contentIds, vdom2) = buildChildren content vdom'
           in (FragmentNode contentIds, vdom2)
        Element tag attrs content ->
          let (contentIds, vdom2) = buildChildren content vdom'
           in (ElementNode tag attrs contentIds, vdom2)
        Text s -> (TextNode s, vdom')
   in (i, vdom'' {_tree = insert i node (_tree vdom'')})

buildChildren :: (Foldable t) => t Html -> VirtualDom -> ([Int], VirtualDom)
buildChildren content vdom =
  foldr'
    ( \c (idAcc, vdomAcc) ->
        let (i, vdomAcc') = buildHtml c vdomAcc
         in (i : idAcc, vdomAcc')
    )
    ([], vdom)
    content

data Mutation = SetText Int String
  deriving (Eq, Show)

rebuildHtml :: Int -> VirtualDom -> ([Mutation], VirtualDom)
rebuildHtml i vdom = case _tree vdom ! i of
  ComponentNode content hooks contentId ->
    let contentNode = _tree vdom ! contentId
        (contentHtml, _, hooks') = runComponent content i 0 hooks
        vdom' = vdom {_tree = insert i (ComponentNode content hooks' contentId) (_tree vdom)}
     in rebuildHtml' contentId contentHtml contentNode vdom'
  _ -> error ""

rebuildHtml' :: Int -> Html -> Node -> VirtualDom -> ([Mutation], VirtualDom)
rebuildHtml' i html node vdom = case html of
  Text s -> case node of
    TextNode lastS -> ([SetText i s | s /= lastS], vdom)
    _ -> error ""
  Element tag attrs elemContent -> case node of
    -- TODO diff last attrs
    ElementNode lastTag _ childIds ->
      if tag == lastTag
        then
          let (mutations, vdom') =
                foldr'
                  ( \(childId, childHtml) (acc, accVdom) ->
                      let childNode = _tree accVdom ! childId
                          (ms, accVdom') = rebuildHtml' childId childHtml childNode accVdom
                       in (acc ++ ms, accVdom')
                  )
                  ([], vdom)
                  (zip childIds elemContent)
           in (mutations, vdom' {_tree = insert i (ElementNode tag attrs childIds) (_tree vdom')})
        else error ""
    _ -> error ""
  _ -> error ""

handle :: Int -> String -> VirtualDom -> VirtualDom
handle i event vdom = foldr' update vdom (handle' i event vdom)

handle' :: Int -> [Char] -> VirtualDom -> [Update]
handle' i event vdom = case _tree vdom ! i of
  ElementNode _ attrs _ ->
    fromMaybe [] $
      findIndex (\(HtmlAttribute n _) -> n == event) attrs
        >>= \x -> case attrs !! x of
          HtmlAttribute _ (Handler s) ->
            let (_, updates) = runScope s i 0
             in Just updates
          _ -> Nothing
  _ -> error "TODO"

update :: Update -> VirtualDom -> VirtualDom
update (Update i idx dyn) vdom =
  vdom
    { _tree =
        adjust
          ( \case
              ComponentNode html hooks childId ->
                ComponentNode html (replaceAt idx dyn hooks) childId
              _ -> error ""
          )
          i
          (_tree vdom)
    }

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceAt (n - 1) newVal xs
