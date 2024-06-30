module Main where

import Conduit
import ViewMonad

app :: (Monad m) => Html m
app = component_ $ do
  (count, setCount) <- useState (0 :: Int)

  return $
    div_
      []
      [ text_ $ "Useful files: " ++ show count,
        button_ [on_ "click" $ setCount (count + 1)] [text_ "Clone repo!"],
        button_ [on_ "click" $ setCount (count - 1)] [text_ "Download meme!"]
      ]

main :: IO ()
main = do
  let events = [(3, "onclick"), (3, "onclick")]
  mutations <- runConduit $ mapM_ yield events .| stream app .| sinkList
  print mutations
