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
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
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

useState :: (Monad m) => Lens' s a -> Component m s (a, a -> Scope ())
useState f = Component (\i state -> pure ((state ^. f, (\new -> Scope (\i _ -> error "TODO"))), state))
