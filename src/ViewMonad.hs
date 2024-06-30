{-# LANGUAGE DeriveFunctor #-}

module ViewMonad
  ( Node (..),
    Tree,
    Html (..),
    component_,
    element_,
    text_,
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
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.IntMap (IntMap, insert, (!))
import Data.Maybe (fromMaybe)

data Html = HtmlComponent (Component Html) | Fragment [Html] | Element String [Html] | Text String
  deriving (Show)

component_ :: Component Html -> Html
component_ = HtmlComponent

element_ :: String -> [Html] -> Html
element_ = Element

text_ :: String -> Html
text_ = Text

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
     in rebuildHtml' contentId contentHtml contentNode vdom'
  _ -> error ""

rebuildHtml' :: Int -> Html -> Node -> VirtualDom -> ([Mutation], VirtualDom)
rebuildHtml' i html node vdom = case html of
  Text s -> case node of
    TextNode lastS -> ([SetText i s | s /= lastS], vdom)
    _ -> error ""
  Element tag elemContent -> case node of
    ElementNode lastTag childIds ->
      if tag == lastTag
        then
          foldr
            ( \(childId, childHtml) (acc, accVdom) ->
                let childNode = _tree accVdom ! childId
                    (ms, accVdom') = rebuildHtml' childId childHtml childNode accVdom
                 in (acc ++ ms, accVdom')
            )
            ([], vdom)
            (zip childIds elemContent)
        else error ""
    _ -> error ""
  _ -> error ""

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceAt (n - 1) newVal xs