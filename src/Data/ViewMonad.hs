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
  )
where

import Control.Lens
import Control.Monad (ap)
import Data.Typeable

data Update where Update :: (Typeable s) => Int -> !a -> !(Lens' s a) -> Update

newtype Scope a = Scope {runScope :: Int -> (a, [Update])}
  deriving (Functor)

instance Applicative Scope where
  pure a = Scope (\_ -> (a, []))

  (<*>) = ap

instance Monad Scope where
  (>>=) a f =
    Scope
      ( \i ->
          let (a', updates1) = runScope a i
              (b, updates2) = runScope (f a') i
           in (b, updates1 ++ updates2)
      )

newtype Component m s a = Component
  { runComponent :: Int -> s -> m (a, s)
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

useState :: (Monad m, Typeable s) => Lens' s a -> Component m s (a, a -> Scope ())
useState f =
  Component
    ( \i state ->
        pure
          ( ( state ^. f,
              (\new -> Scope (\_ -> ((), [Update i new f])))
            ),
            state
          )
    )
