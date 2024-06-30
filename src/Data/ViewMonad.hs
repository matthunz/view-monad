{-# LANGUAGE DeriveFunctor #-}

-- |
-- Copyright   :  (c) Matt Hunzinger 2024
-- License     :  BSD-3
-- Maintainer  : matthunz2@gmail.com
module Data.ViewMonad
  ( Component (..),
    useHook,
    useState,
    Update (..),
    Scope (..),
  )
where

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

newtype Component m a = Component
  { runComponent :: Int -> Int -> [Dynamic] -> m (a, Int, [Dynamic])
  }
  deriving (Functor)

instance Show (Component m a) where
  show _ = "Component"

instance (Monad m) => Applicative (Component m) where
  pure a = Component (\_ i hooks -> pure (a, i, hooks))
  (<*>) = ap

instance (Monad m) => Monad (Component m) where
  (>>=) a f =
    Component
      ( \i idx hooks -> do
          (a', idx', hooks') <- runComponent a i idx hooks
          (b, idx'', hooks'') <- runComponent (f a') i idx' hooks'
          return (b, idx'', hooks'')
      )

useHook :: (Applicative m) => (Typeable a) => (Int -> Int -> a) -> Component m a
useHook f =
  Component
    ( \i idx hooks ->
        if idx >= length hooks
          then let x = f i idx in pure (x, idx + 1, hooks ++ [toDyn x])
          else
            let val = hooks !! idx
             in pure (fromMaybe (error "TODO") (fromDynamic val), idx + 1, hooks)
    )

data State a = State !Int !Int !a

useState :: (Monad m) => (Typeable a) => a -> Component m (a, a -> Scope ())
useState s =
  ( \(State i idx s') ->
      ( s',
        \new -> Scope (\_ _ -> ((), [Update i idx (toDyn (State i idx new))]))
      )
  )
    <$> useHook (\i idx -> State i idx s)
