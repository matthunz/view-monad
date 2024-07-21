# view-monad

An experimental declarative UI framework for Haskell inspired by [React](https://github.com/facebook/react).

```hs
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import ViewMonad

data Counter = Counter
  { _countHook :: UseState Int,
    _countHook' :: UseMemo Int Int,
    _effectHook :: UseMemo Int ()
  }

makeLenses ''Counter

app :: (MonadIO m) => View m
app = componentV (Counter mkState mkMemo mkMemo) $ do
  (count, setCount) <- useState countHook 0

  count' <- useMemo countHook' count $ \c -> pure $ c * 2

  _ <- useMemo effectHook count $ \c -> setCount (c + 1)

  liftIO $ print count'

  return []
```

The plan is to eventually integrate this as a virtual dom for [Tauri](https://tauri.app) via [conduct](https://github.com/matthunz/conduct/).
Native UI should also be possible with [Blitz](https://github.com/DioxusLabs/blitz) or [Masonry](https://github.com/linebender/xilem).
