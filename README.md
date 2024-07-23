# view-monad

An experimental declarative UI framework for Haskell inspired by [React](https://github.com/facebook/react).

```hs
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import ViewMonad

data Counter = Counter
  { _countHook :: UseState Int,
    _countHook' :: UseMemo Int Int,
    _effectHook :: UseEffect Int
  }

makeLenses ''Counter

counter :: (MonadIO m) => View m
counter = componentV (Counter mkState mkMemo mkEffect) $ do
  (count, setCount) <- useState countHook 0

  count' <- useMemo countHook' count $ \c -> pure $ c * 2

  useEffect effectHook count $ \c -> setCount $ c + 1

  useUnmount . liftIO $ print "Unmounted!"

  liftIO $ print count'

  return []

app :: (MonadIO m) => View m
app = componentV () $ return [counter, counter]
```

The plan is to eventually integrate this as a virtual dom for [Tauri](https://tauri.app) via [conduct](https://github.com/matthunz/conduct/).
Native UI should also be possible with [Blitz](https://github.com/DioxusLabs/blitz) or [Masonry](https://github.com/linebender/xilem).
