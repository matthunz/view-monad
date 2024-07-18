# view-monad

An experimental declarative UI framework for Haskell inspired by [React](https://github.com/facebook/react).

```hs
import Control.Lens
import ViewMonad

data Counter = Counter
  { _counter :: Int,
    _output :: Memo Int Int
  }

makeLenses ''Counter

app :: (Monad m) => Html m
app = component_ (Counter 0 memo) $ do
  (count, setCount) <- useState counter

  count' <- useMemo output count $ \x -> pure $ x * 2

  return $
    div_
      []
      [ text_ $ "Useful files: " ++ show count',
        button_ [on_ "click" $ setCount (count + 1)] [text_ "Clone repo!"],
        button_ [on_ "click" $ setCount (count - 1)] [text_ "Download meme!"]
      ]
```

The plan is to eventually integrate this as a virtual dom for [Tauri](https://tauri.app) via [conduct](https://github.com/matthunz/conduct/).
Native UI should also be possible with [Blitz](https://github.com/DioxusLabs/blitz) or [Masonry](https://github.com/linebender/xilem).
