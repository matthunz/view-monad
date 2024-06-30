{-# LANGUAGE DeriveFunctor #-}

module ViewMonad
  ( Node (..),
    Tree,
    Html (..),
    Component,
    useHook,
    useState,
    VirtualDom,
    mkVirtualDom,
    buildHtml,
    rebuildHtml,
  )
where

import Control.Monad (ap)
import Data.Dynamic (Dynamic, Typeable, fromDyn, fromDynamic, toDyn)
import Data.IntMap (IntMap, insert, (!))
import Data.Maybe (fromMaybe)

data Html = HtmlComponent (Component Html) | Fragment [Html] | Element String [Html] | Text String
  deriving (Show)

data Node
  = ComponentNode (Component Html) [Dynamic] Int
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
        HtmlComponent content ->
          let (contentHtml, _, hooks) = runComponent content i 0 []
              (contentId, vdom2) = buildHtml contentHtml vdom'
           in (ComponentNode content hooks contentId, vdom2)
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

newtype Component a = Component
  { runComponent :: Int -> Int -> [Dynamic] -> (a, Int, [Dynamic])
  }
  deriving (Functor)

instance Show (Component a) where
  show _ = "Component"

instance Applicative Component where
  pure a = Component (\_ i hooks -> (a, i, hooks))
  (<*>) = ap

instance Monad Component where
  (>>=) a f =
    Component
      ( \i idx hooks ->
          let (a', idx', hooks') = runComponent a i idx hooks
              (b, idx'', hooks'') = runComponent (f a') i idx' hooks'
           in (b, idx'', hooks'')
      )

useHook :: (Typeable a) => (Int -> Int -> a) -> Component a
useHook f =
  Component
    ( \i idx hooks ->
        if idx >= length hooks
          then let x = f i idx in (x, idx + 1, hooks ++ [toDyn x])
          else
            let val = hooks !! idx
             in (fromMaybe (error "TODO") (fromDynamic val), idx + 1, hooks)
    )

data State a = State Int Int a

useState :: (Typeable a) => a -> Component (a, a -> Component ())
useState s =
  ( \(State i idx s') ->
      ( s',
        \new ->
          Component
            ( \_ _ hooks ->
                let x = replaceAt idx (toDyn (State i idx new)) hooks
                 in ((), idx, x)
            )
      )
  )
    <$> useHook (\i idx -> State i idx s)

data Mutation = SetText Int String
  deriving (Show)

rebuildHtml :: Int -> VirtualDom -> ([Mutation], VirtualDom)
rebuildHtml i vdom = case _tree vdom ! i of
  ComponentNode content hooks contentId ->
    let contentNode = _tree vdom ! contentId
        (contentHtml, _, hooks') = runComponent content i 0 hooks
        vdom' = vdom {_tree = insert i (ComponentNode content hooks' contentId) (_tree vdom)}
     in case contentHtml of
          Text s -> case contentNode of
            TextNode lastS -> ([SetText contentId s | s /= lastS], vdom')
            _ -> error ""
          _ -> error ""
  _ -> error ""

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceAt (n - 1) newVal xs