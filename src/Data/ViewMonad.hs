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
    useState,
    Update (..),
    Scope (..),
    Memo,
    memo,
    useMemo,
  )
where

import Control.Lens
import Control.Monad (ap)
import Data.Typeable

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

useState :: (Monad m, Typeable s) => Lens' s a -> Component m s (a, a -> Scope m ())
useState f =
  Component
    ( \i state ->
        pure
          ( ( state ^. f,
              (\new -> Scope (\_ -> pure ((), [Update i new f])))
            ),
            state
          )
    )

data Memo d a = Memo (Maybe (d, a))

memo :: Memo d a
memo = Memo Nothing

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
