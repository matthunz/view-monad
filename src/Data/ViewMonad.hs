{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright   :  (c) Matt Hunzinger 2024
-- License     :  BSD-3
-- Maintainer  :  matthunz2@gmail.com
module Data.ViewMonad
  ( Component (..),
    Update (..),
    Scope (..),
    UseState,
    mkState,
    useState,
    UseMemo,
    mkMemo,
    useMemo,
    UseEffect,
    mkEffect,
    useEffect,
    useUnmount,
    View,
    componentV,
    UserInterface,
    mkUI,
    buildUI,
    rebuildUI,
    updateUI,
  )
where

import Control.Lens (Lens', set, (^.))
import Control.Monad (ap, foldM)
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable, cast)

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

instance (MonadIO m) => MonadIO (Scope m) where
  liftIO io = Scope (\_ -> do a <- liftIO io; pure (a, []))

instance MonadTrans Scope where
  lift m = Scope (\_ -> do a <- m; pure (a, []))

data Component s m a = Component
  { runComponent' :: Int -> s -> Scope m (a, s),
    removeComponent :: Int -> s -> Scope m ()
  }
  deriving (Functor)

instance (Monad m) => Applicative (Component s m) where
  pure a = Component (\_ state -> pure (a, state)) (\_ _ -> pure ())
  (<*>) = ap

instance (Monad m) => Monad (Component s m) where
  (>>=) a f =
    Component
      ( \i state -> do
          (a', state') <- runComponent' a i state
          (b, state'') <- runComponent' (f a') i state'
          return (b, state'')
      )
      ( \i state -> do
          removeComponent a i state
      )

instance (MonadIO m) => MonadIO (Component s m) where
  liftIO io = Component (\_ state -> do a <- liftIO io; pure (a, state)) (\_ _ -> pure ())

instance MonadTrans (Component s) where
  lift m = Component (\_ state -> Scope (\_ -> do a <- m; pure ((a, state), []))) (\_ _ -> pure ())

-- | Run a `Component` and its inner `Scope`.
runComponent :: (Functor m) => Component s m a -> Int -> s -> m (a, s, [Update])
runComponent c i s = (\((a, s'), us) -> (a, s', us)) <$> runScope (runComponent' c i s) i

newtype UseState a = UseState (Maybe a)

mkState :: UseState a
mkState = UseState Nothing

useState :: (Monad m, Typeable s) => Lens' s (UseState a) -> a -> Component s m (a, a -> Scope m ())
useState l val =
  let maybeLens f (UseState (Just x)) = UseState . Just <$> f x
      maybeLens _ (UseState Nothing) = error "TODO"
   in Component
        ( \i state ->
            let (UseState cell) = state ^. l
                setter new = Scope (\_ -> pure ((), [Update i new $ l . maybeLens]))
             in pure $ case cell of
                  Just cached -> ((cached, setter), state)
                  Nothing -> ((val, setter), set l (UseState $ Just val) state)
        )
        (\_ _ -> pure ())

-- | Hook for `useMemo`.
newtype UseMemo d a = UseMemo (Maybe (d, a))

-- | Create a new `UseMemo`.
mkMemo :: UseMemo d a
mkMemo = UseMemo Nothing

-- | Use a memoized value, only recomputing when the provided dependency is changed.
useMemo ::
  (Eq d, Monad m, Typeable s) =>
  Lens' s (UseMemo d a) ->
  d ->
  (d -> Scope m a) ->
  Component s m a
useMemo l dep f =
  Component
    ( \i state ->
        let runner =
              Scope
                ( \_ -> do
                    (a, updates) <- runScope (f dep) i
                    return ((a, set l (UseMemo $ Just (dep, a)) state), updates)
                )
         in case state ^. l of
              (UseMemo (Just (cachedDep, cached))) ->
                if cachedDep == dep
                  then pure (cached, state)
                  else runner
              (UseMemo Nothing) -> runner
    )
    (\_ _ -> pure ())

-- | Hook for `useEffect`.
newtype UseEffect d = UseEffect (Maybe d)

-- | Create a new `UseEffect`.
mkEffect :: UseEffect d
mkEffect = UseEffect Nothing

-- | Use an effect, only running when the provided dependency is changed.
useEffect ::
  (Eq d, Monad m, Typeable s) =>
  Lens' s (UseEffect d) ->
  d ->
  (d -> Scope m ()) ->
  Component s m ()
useEffect l dep f =
  Component
    ( \i state ->
        let runner =
              Scope
                ( \_ -> do
                    (a, updates) <- runScope (f dep) i
                    return ((a, set l (UseEffect $ Just dep) state), updates)
                )
         in case state ^. l of
              (UseEffect (Just cached)) ->
                if cached == dep
                  then pure ((), state)
                  else runner
              (UseEffect Nothing) -> runner
    )
    (\_ _ -> pure ())

-- | Use a computation that runs when this component is unmounted.
useUnmount :: (Monad m) => Scope m () -> Component s m ()
useUnmount f = Component (\_ s -> pure ((), s)) (\_ _ -> f)

data View m where
  ComponentV :: (Typeable s) => s -> Component s m [View m] -> View m

componentV :: (Typeable s) => s -> Component s m [View m] -> View m
componentV = ComponentV

data ViewNode m = ViewNode (View m) [Int]

data UserInterface m = UserInterface
  { _views :: IntMap (ViewNode m),
    _nextId :: Int
  }

mkUI :: UserInterface m
mkUI = UserInterface mempty 0

buildUI :: (Monad m) => View m -> UserInterface m -> m (Int, [Update], UserInterface m)
buildUI (ComponentV s c) ui = do
  let i = _nextId ui
      ui' = ui {_nextId = i + 1}
  (vs, s', updates) <- runComponent c i s
  (childIds, updates', ui'') <-
    foldM
      ( \(idAcc, updateAcc, uiAcc) v -> do
          (childId, updates2, ui'') <- buildUI v uiAcc
          return (childId : idAcc, updateAcc ++ updates2, ui'')
      )
      ([], updates, ui')
      vs
  return
    ( i,
      updates',
      ui''
        { _views = IntMap.insert i (ViewNode (ComponentV s' c) childIds) (_views ui'')
        }
    )

rebuildUI :: (Monad m) => Int -> UserInterface m -> m (Int, [Update], UserInterface m)
rebuildUI i ui = case IntMap.lookup i (_views ui) of
  Just (ViewNode v childIds) -> rebuildView v (ViewNode v childIds) i ui
  Nothing -> error "TODO"

rebuildView :: (Monad m) => View m -> ViewNode m -> Int -> UserInterface m -> m (Int, [Update], UserInterface m)
rebuildView (ComponentV s c) (ViewNode (ComponentV lastS lastC) childIds) i ui = case cast lastS of
  Just s' -> do
    (vs, s'', updates) <- runComponent c i s'
    (childIds', updates', ui') <-
      foldM
        ( \(idAcc, updateAcc, uiAcc) (childId, v) -> do
            (childId', updates2, uiAcc') <- rebuildView v (_views uiAcc IntMap.! childId) childId uiAcc
            return (idAcc ++ [childId'], updateAcc ++ updates2, uiAcc')
        )
        ([], updates, ui)
        (zip childIds vs)
    return
      ( i,
        updates',
        ui' {_views = IntMap.insert i (ViewNode (ComponentV s'' c) childIds') (_views ui')}
      )
  Nothing -> do
    ((), updates) <- runScope (removeComponent lastC i lastS) i
    (i', updates2, ui') <- buildUI (ComponentV s c) ui
    return (i', updates ++ updates2, ui')

updateUI :: Update -> UserInterface m -> UserInterface m
updateUI (Update i val l) ui =
  ui
    { _views =
        IntMap.adjust
          ( \case
              ViewNode (ComponentV state content) childId ->
                ViewNode
                  ( ComponentV
                      ( fromMaybe (error "TODO") $
                          cast (set l val (fromMaybe (error "TODO") $ cast state))
                      )
                      content
                  )
                  childId
          )
          i
          (_views ui)
    }
