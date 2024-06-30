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
    NodeHandle (..),
    root,
    ElementHandle,
    find,
    click,
  )
where

import Control.Monad (foldM)
import Data.Dynamic (Dynamic)
import Data.Foldable (foldr')
import Data.IntMap (IntMap, adjust, insert, (!))
import Data.List (findIndex)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.ViewMonad
import Data.ViewMonad.Html

data Node m
  = ComponentNode !(Component m (Html m)) ![Dynamic] !Int
  | FragmentNode ![Int]
  | ElementNode !String ![HtmlAttribute] ![Int]
  | TextNode !String
  deriving (Show)

type Tree m = IntMap (Node m)

foldTree' :: (Int -> Node m -> a -> a) -> a -> Int -> Tree m -> a
foldTree' f acc i tree =
  let node = tree ! i
      acc' = f i node acc
   in case node of
        ComponentNode _ _ childId -> foldTree' f acc' childId tree
        FragmentNode childIds ->
          foldr'
            ( \childId acc'' ->
                foldTree' f acc'' childId tree
            )
            acc'
            childIds
        ElementNode _ _ childIds ->
          foldr'
            ( \childId acc'' ->
                foldTree' f acc'' childId tree
            )
            acc'
            childIds
        TextNode _ -> acc'

data VirtualDom m = VirtualDom
  { _nextId :: !Int,
    _tree :: !(Tree m)
  }
  deriving (Show)

mkVirtualDom :: VirtualDom m
mkVirtualDom = VirtualDom 0 mempty

buildHtml :: (Monad m) => Html m -> VirtualDom m -> m (Int, VirtualDom m)
buildHtml html vdom = do
  let i = _nextId vdom
      vdom' = vdom {_nextId = _nextId vdom + 1}
  (node, vdom'') <- case html of
    HtmlComponent content -> do
      (contentHtml, _, hooks) <- runComponent content i 0 []
      (contentId, vdom2) <- buildHtml contentHtml vdom'
      return (ComponentNode content hooks contentId, vdom2)
    Fragment content -> do
      (contentIds, vdom2) <- buildChildren content vdom'
      return (FragmentNode contentIds, vdom2)
    Element tag attrs content -> do
      (contentIds, vdom2) <- buildChildren content vdom'
      return (ElementNode tag attrs contentIds, vdom2)
    Text s -> pure (TextNode s, vdom')
  return (i, vdom'' {_tree = insert i node (_tree vdom'')})

buildChildren :: (Foldable t, Monad m) => t (Html m) -> VirtualDom m -> m ([Int], VirtualDom m)
buildChildren content vdom =
  foldM
    ( \(idAcc, vdomAcc) c -> do
        (i, vdomAcc') <- buildHtml c vdomAcc
        return (idAcc ++ [i], vdomAcc')
    )
    ([], vdom)
    content

data Mutation = SetText Int String
  deriving (Eq, Show)

rebuildHtml :: (Monad m) => Int -> VirtualDom m -> m ([Mutation], VirtualDom m)
rebuildHtml i vdom = case _tree vdom ! i of
  ComponentNode content hooks contentId -> do
    let contentNode = _tree vdom ! contentId
    (contentHtml, _, hooks') <- runComponent content i 0 hooks
    let vdom' = vdom {_tree = insert i (ComponentNode content hooks' contentId) (_tree vdom)}
    return $ rebuildHtml' contentId contentHtml contentNode vdom'
  _ -> error ""

rebuildHtml' :: Int -> Html m -> Node m -> VirtualDom m -> ([Mutation], VirtualDom m)
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

handle :: Int -> String -> VirtualDom m -> VirtualDom m
handle i event vdom = foldr' update vdom (handle' i event vdom)

handle' :: Int -> [Char] -> VirtualDom m -> [Update]
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

update :: Update -> VirtualDom m -> VirtualDom m
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

data NodeHandle m = NodeHandle Int (Node m) (VirtualDom m)

root :: VirtualDom m -> NodeHandle m
root vdom = NodeHandle 0 (_tree vdom ! 0) vdom

newtype ElementHandle m = ElementHandle (NodeHandle m)

find :: String -> NodeHandle m -> Maybe (ElementHandle m)
find tag (NodeHandle i _ vdom) =
  listToMaybe $
    foldTree'
      ( \nodeId n acc -> case n of
          ElementNode t _ _ ->
            if t == tag
              then acc ++ [ElementHandle $ NodeHandle nodeId n vdom]
              else acc
          _ -> acc
      )
      []
      i
      (_tree vdom)

click :: ElementHandle m -> VirtualDom m
click (ElementHandle (NodeHandle i _ vdom)) = handle i "onclick" vdom
