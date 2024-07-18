{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright   :  (c) Matt Hunzinger 2024
-- License     :  BSD-3
-- Maintainer  :  matthunz2@gmail.com
module Data.ViewMonad.VirtualDom
  ( Node (..),
    Tree,
    VirtualDom,
    Mutation (..),
    mkVirtualDom,
    buildHtml,
    buildHtml',
    rebuildHtml,
    handle,
    handle',
    update,
    NodeHandle (..),
    root,
    ElementHandle,
    find,
    click,
    stream,
  )
where

import Conduit
import Control.Lens
import Control.Monad (foldM)
import Data.Foldable (foldr')
import Data.IntMap (IntMap, adjust, insert, (!))
import Data.List (findIndex)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Typeable
import Data.ViewMonad
import Data.ViewMonad.Html

data Node m
  = ComponentNode !(DynComponent m (Html m)) !Int
  | FragmentNode ![Int]
  | ElementNode !String ![HtmlAttribute m] ![Int]
  | TextNode !String

instance Show (Node m) where
  show (ComponentNode _ c) = "ComponentNode " ++ show c
  show (FragmentNode cs) = "FragmentNode" ++ show cs
  show (ElementNode t _ cs) = "ElementNode " ++ t ++ " " ++ show cs
  show (TextNode s) = "TextNode " ++ s

type Tree m = IntMap (Node m)

foldTree' :: (Int -> Node m -> a -> a) -> a -> Int -> Tree m -> a
foldTree' f acc i tree =
  let node = tree ! i
      acc' = f i node acc
   in case node of
        ComponentNode _ childId -> foldTree' f acc' childId tree
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

-- | Mutation to apply to the HTML DOM.
data Mutation
  = InsertElement Int Int String
  | InsertText Int Int String
  | SetText Int String
  deriving (Eq, Show)

-- | Build the root HTML.
buildHtml :: (Monad m) => Html m -> VirtualDom m -> m ([Mutation], [Update], VirtualDom m)
buildHtml html vdom = do
  (mutations, _, updates, vdom') <- buildHtml' html 0 vdom
  return (mutations, updates, vdom')

-- | Build HTML into a given parent ID.
buildHtml' :: (Monad m) => Html m -> Int -> VirtualDom m -> m ([Mutation], Int, [Update], VirtualDom m)
buildHtml' html parentId vdom = do
  let i = _nextId vdom
      vdom' = vdom {_nextId = _nextId vdom + 1}
  (mutations, node, updates, vdom'') <- case html of
    HtmlComponent (DynComponent state content) -> do
      ((contentHtml, state'), updates) <- runScope (runComponent content i state) i
      (ms, contentId, updates2, vdom2) <- buildHtml' contentHtml i vdom'
      return (ms, ComponentNode (DynComponent state' content) contentId, updates ++ updates2, vdom2)
    Fragment content -> do
      (ms, contentIds, updates, vdom2) <- buildChildren content i vdom'
      return (ms, FragmentNode contentIds, updates, vdom2)
    Element tag attrs content -> do
      (ms, contentIds, updates, vdom2) <- buildChildren content i vdom'
      return (InsertElement parentId i tag : ms, ElementNode tag attrs contentIds, updates, vdom2)
    Text s -> pure ([InsertText parentId i s], TextNode s, [], vdom')
  return (mutations, i, updates, vdom'' {_tree = insert i node (_tree vdom'')})

-- | Build the content of an HTML item.
buildChildren ::
  (Foldable t, Monad m) =>
  t (Html m) ->
  Int ->
  VirtualDom m ->
  m ([Mutation], [Int], [Update], VirtualDom m)
buildChildren content parentId vdom =
  foldM
    ( \(mAcc, idAcc, updateAcc, vdomAcc) c -> do
        (ms, i, updates, vdomAcc') <- buildHtml' c parentId vdomAcc
        return (mAcc ++ ms, idAcc ++ [i], updateAcc ++ updates, vdomAcc')
    )
    ([], [], [], vdom)
    content

rebuildHtml :: (Monad m) => Int -> VirtualDom m -> m ([Mutation], [Update], VirtualDom m)
rebuildHtml i vdom = case _tree vdom ! i of
  ComponentNode (DynComponent state content) contentId -> do
    let contentNode = _tree vdom ! contentId
    ((contentHtml, state'), updates) <- runScope (runComponent content i state) i
    let vdom' = vdom {_tree = insert i (ComponentNode (DynComponent state' content) contentId) (_tree vdom)}
        (ms, vdom'') = rebuildHtml' contentId contentHtml contentNode vdom'
    return (ms, updates, vdom'')
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

handle :: (Monad m) => Int -> String -> VirtualDom m -> m (VirtualDom m)
handle i event vdom = do
  updates <- handle' i event vdom
  return $ foldr update vdom updates

handle' :: (Monad m) => Int -> [Char] -> VirtualDom m -> m [Update]
handle' i event vdom = case _tree vdom ! i of
  ElementNode _ attrs _ -> do
    let res =
          findIndex (\(HtmlAttribute n _) -> n == event) attrs
            >>= \x -> case attrs !! x of
              HtmlAttribute _ (Handler s) -> Just $ runScope s i
              _ -> Nothing
    case res of
      Just s -> fmap snd s
      Nothing -> pure []
  _ -> pure []

update :: Update -> VirtualDom m -> VirtualDom m
update (Update i val l) vdom =
  vdom
    { _tree =
        adjust
          ( \case
              ComponentNode (DynComponent state content) childId ->
                ComponentNode
                  ( DynComponent
                      ( fromMaybe (error "TODO") $
                          cast (set l val (fromMaybe (error "TODO") $ cast state))
                      )
                      content
                  )
                  childId
              _ -> error ""
          )
          i
          (_tree vdom)
    }

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

click :: (Monad m) => ElementHandle m -> m (VirtualDom m)
click (ElementHandle (NodeHandle i _ vdom)) = handle i "onclick" vdom

stream :: (Monad m) => Html m -> ConduitT (Int, String) Mutation m ()
stream html = do
  (mutations, updates, vdom) <- lift $ buildHtml html mkVirtualDom
  mapM_ yield mutations
  stream' vdom

stream' :: (Monad m) => VirtualDom m -> ConduitT (Int, String) Mutation m ()
stream' vdom = do
  event <- await
  case event of
    Nothing -> return ()
    Just (i, eventName) -> do
      vdom' <- lift $ handle i eventName vdom
      (mutations, updates, vdom'') <- lift $ rebuildHtml 0 vdom'
      mapM_ yield mutations
      stream' vdom''
