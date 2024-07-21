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
    View,
    componentV,
    UserInterface,
    mkUI,
    buildUI,
    rebuildUI,
    updateUI,
  )
where

import Control.Lens
import Control.Monad (ap, foldM)
import Control.Monad.Trans
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
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

instance (MonadIO m) => MonadIO (Scope m) where
  liftIO io = Scope (\_ -> do a <- liftIO io; pure (a, []))

instance MonadTrans Scope where
  lift m = Scope (\_ -> do a <- m; pure (a, []))

newtype Component s m a = Component
  { runComponent :: Int -> s -> Scope m (a, s)
  }
  deriving (Functor)

instance MonadTrans (Component s) where
  lift m = Component (\_ state -> Scope (\_ -> do a <- m; pure ((a, state), [])))

instance Show (Component s m a) where
  show _ = "Component"

instance (Monad m) => Applicative (Component s m) where
  pure a = Component (\_ state -> pure (a, state))
  (<*>) = ap

instance (Monad m) => Monad (Component s m) where
  (>>=) a f =
    Component
      ( \i state -> do
          (a', state') <- runComponent a i state
          (b, state'') <- runComponent (f a') i state'
          return (b, state'')
      )

instance (MonadIO m) => MonadIO (Component s m) where
  liftIO io = Component (\_ state -> do a <- liftIO io; pure (a, state))

maybeLens :: Lens' (UseState a) a
maybeLens f (UseState (Just x)) = UseState . Just <$> f x
maybeLens _ (UseState Nothing) = error "TODO"

newtype UseState a = UseState (Maybe a)

mkState :: UseState a
mkState = UseState Nothing

useState :: (Monad m, Typeable s) => Lens' s (UseState a) -> a -> Component s m (a, a -> Scope m ())
useState l val =
  Component
    ( \i state ->
        let (UseState cell) = state ^. l
            setter new = Scope (\_ -> pure ((), [Update i new $ l . maybeLens]))
         in pure $ case cell of
              Just cached -> ((cached, setter), state)
              Nothing -> ((val, setter), set l (UseState $ Just val) state)
    )

newtype UseMemo d a = UseMemo (Maybe (d, a))

mkMemo :: UseMemo d a
mkMemo = UseMemo Nothing

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
  ((vs, s'), updates) <- runScope (runComponent c i s) i
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
        { _views = IntMap.insert (_nextId ui) (ViewNode (ComponentV s' c) childIds) (_views ui)
        }
    )

rebuildUI :: (Monad m) => Int -> UserInterface m -> m ([Update], UserInterface m)
rebuildUI i ui = case IntMap.lookup i (_views ui) of
  Just (ViewNode v childIds) -> rebuildView v (ViewNode v childIds) i ui
  Nothing -> error "TODO"

rebuildView :: (Monad m) => View m -> ViewNode m -> Int -> UserInterface m -> m ([Update], UserInterface m)
rebuildView (ComponentV s c) (ViewNode (ComponentV lastS lastC) childIds) i ui = case cast lastS of
  Just s' -> do
    let scope = runComponent c i s'
    ((vs, s''), updates) <- runScope scope i
    (updates', ui') <-
      foldM
        ( \(updateAcc, uiAcc) (childId, v) -> do
            (updates2, uiAcc') <- rebuildView v (_views uiAcc IntMap.! childId) childId uiAcc
            return (updateAcc ++ updates2, uiAcc')
        )
        (updates, ui)
        (zip childIds vs)
    return
      ( updates',
        ui' {_views = IntMap.insert i (ViewNode (ComponentV s'' c) childIds) (_views ui')}
      )
  Nothing -> error ""

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
