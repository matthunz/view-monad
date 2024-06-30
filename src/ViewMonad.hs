{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module ViewMonad
  ( Node (..),
    Tree,
    HtmlAttribute (..),
    on_,
    Html (..),
    component_,
    element_,
    text_,
    div_,
    button_,
    Component,
    useHook,
    useState,
    VirtualDom,
    mkVirtualDom,
    buildHtml,
    rebuildHtml,
    handle,
    handle',
    update,
  )
where

import Control.Monad (ap)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.Foldable (foldr')
import Data.IntMap (IntMap, adjust, insert, (!))
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

data Update = Update !Int !Int !Dynamic

newtype Scope a = Scope {runScope :: Int -> Int -> (a, [Update])}
  deriving (Functor)

instance Applicative Scope where
  pure a = Scope (\_ _ -> (a, []))

  (<*>) = ap

instance Monad Scope where
  (>>=) a f =
    Scope
      ( \i idx ->
          let (a', updates1) = runScope a i idx
              (b, updates2) = runScope (f a') i idx
           in (b, updates1 ++ updates2)
      )

data HtmlAttributeValue = TextValue String | Handler (Scope ())

instance Show HtmlAttributeValue where
  show (TextValue s) = s
  show (Handler _) = "<<Handler>>"

data HtmlAttribute = HtmlAttribute String HtmlAttributeValue
  deriving (Show)

on_ :: String -> Scope () -> HtmlAttribute
on_ n s = HtmlAttribute ("on" ++ n) (Handler s)

data Html = HtmlComponent !(Component Html) | Fragment ![Html] | Element !String ![HtmlAttribute] ![Html] | Text !String
  deriving (Show)

component_ :: Component Html -> Html
component_ = HtmlComponent

element_ :: String -> [HtmlAttribute] -> [Html] -> Html
element_ = Element

div_ :: [HtmlAttribute] -> [Html] -> Html
div_ = element_ "div"

button_ :: [HtmlAttribute] -> [Html] -> Html
button_ = element_ "button"

text_ :: String -> Html
text_ = Text

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

data State a = State !Int !Int !a

useState :: (Typeable a) => a -> Component (a, a -> Scope ())
useState s =
  ( \(State i idx s') ->
      ( s',
        \new -> Scope (\_ _ -> ((), [Update i idx (toDyn (State i idx new))]))
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
  Element tag attrs elemContent -> case node of
    ElementNode lastTag lastAttrs childIds ->
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
