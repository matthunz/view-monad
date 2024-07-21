{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright   :  (c) Matt Hunzinger 2024
-- License     :  BSD-3
-- Maintainer  :  matthunz2@gmail.com
module Data.ViewMonad
  ( Component (..),
    DynComponent (..),
    Update (..),
    Scope (..),
    State,
    mkState,
    useState,
    Memo,
    mkMemo,
    useMemo,
    View,
    componentV,
    UserInterface,
    mkUI,
    buildUI
  )
where

import Control.Lens
import Control.Monad (ap, foldM)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Typeable

-- | Update to the `VirtualDom`
data Update where Update :: (Typeable s) => Int -> !a -> !(Lens' s a) -> Update

newtype Scope m a = Scope {runScope :: Int -> m (a, [Update])}
  deriving (Functor)

instance (Monad m) => Applicative (Scope m) where
  pure a = Scope (\_ -> pure (a, []))

  (<*>) = ap

instance (Monad m) => Monad (Scope m) where
  (>>=) a f =
    Scope
      ( \i -> do
          (a', updates1) <- runScope a i
          (b, updates2) <- runScope (f a') i
          return (b, updates1 ++ updates2)
      )

newtype Component m s a = Component
  { runComponent :: Int -> s -> Scope m (a, s)
  }
  deriving (Functor)

instance Show (Component m s a) where
  show _ = "Component"

instance (Monad m) => Applicative (Component m s) where
  pure a = Component (\_ state -> pure (a, state))
  (<*>) = ap

instance (Monad m) => Monad (Component m s) where
  (>>=) a f =
    Component
      ( \i state -> do
          (a', state') <- runComponent a i state
          (b, state'') <- runComponent (f a') i state'
          return (b, state'')
      )

data DynComponent m a where
  DynComponent :: (Typeable s) => s -> Component m s a -> DynComponent m a

maybeLens :: Lens' (State a) a
maybeLens f (State (Just x)) = State <$> Just <$> f x
maybeLens _ (State Nothing) = error "TODO"

data State a = State (Maybe a)

mkState :: State a
mkState = State Nothing

useState :: (Monad m, Typeable s) => Lens' s (State a) -> a -> Component m s (a, a -> Scope m ())
useState l val =
  Component
    ( \i state ->
        let (State cell) = state ^. l
            setter = (\new -> Scope (\_ -> pure ((), [Update i new $ l . maybeLens])))
         in pure $ case cell of
              Just cached -> ((cached, setter), state)
              Nothing -> ((val, setter), set l (State $ Just val) state)
    )

data Memo d a = Memo (Maybe (d, a))

mkMemo :: Memo d a
mkMemo = Memo Nothing

useMemo ::
  (Eq d, Monad m, Typeable s) =>
  Lens' s (Memo d a) ->
  d ->
  (d -> Scope m a) ->
  Component m s a
useMemo l dep f =
  Component
    ( \i state ->
        let runner =
              Scope
                ( \_ -> do
                    (a, updates) <- runScope (f dep) i
                    return ((a, set l (Memo $ Just (dep, a)) state), updates)
                )
         in case state ^. l of
              (Memo (Just (cachedDep, cached))) ->
                if cachedDep == dep
                  then pure (cached, state)
                  else runner
              (Memo Nothing) -> runner
    )

data View m where
  ComponentV :: s -> Component m s [View m] -> View m

componentV :: s -> Component m s [View m] -> View m
componentV = ComponentV

runView :: (Monad m) => View m -> Int -> m (View m, [Update], [View m])
runView (ComponentV s c) i = do
  let scope = runComponent c i s
  ((vs, s'), updates) <- runScope scope i
  return (ComponentV s' c, updates, vs)

data ViewNode m = ViewNode (View m) [Int]

data UserInterface m = UserInterface
  { _views :: IntMap (ViewNode m),
    _nextId :: Int
  }

mkUI :: UserInterface m
mkUI = UserInterface mempty 0

buildUI :: (Monad m) => View m -> UserInterface m -> m (UserInterface m, Int, [Update])
buildUI (ComponentV s c) ui = do
  let i = _nextId ui
      ui' = ui {_nextId = i + 1}
  ((vs, s'), updates) <- runScope (runComponent c i s) i
  (childIds, updates', ui'') <-
    foldM
      ( \(idAcc, updateAcc, uiAcc) v -> do
          (ui'', childId, updates2) <- buildUI v uiAcc
          return (childId : idAcc, updateAcc ++ updates2, ui'')
      )
      ([], updates, ui')
      vs
  return
    ( ui''
        { _views = IntMap.insert (_nextId ui) (ViewNode (ComponentV s' c) childIds) (_views ui)
        },
      i,
      updates'
    )
