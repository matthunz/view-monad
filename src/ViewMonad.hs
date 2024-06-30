module ViewMonad (Node (..), Tree, foldTree) where

import Data.IntMap (IntMap, (!?))

data Node
  = Component Int
  | Fragment [Int]
  | Element String [Int]
  | Text String

type Tree = IntMap Node

foldTree :: (Int -> Node -> a -> a) -> a -> Int -> Tree -> a
foldTree f acc idx tree = case tree !? idx of
  Just x -> case x of
    Component i ->
      let acc' = foldTree f (f idx (Component i) acc) idx tree
       in foldTree f acc' i tree
    Fragment is ->
      let acc' = foldTree f (f idx (Fragment is) acc) idx tree
       in foldr (\i acc'' -> foldTree f acc'' i tree) acc' is
    Element name is ->
      let acc' = foldTree f (f idx (Element name is) acc) idx tree
       in foldr (\i acc'' -> foldTree f acc'' i tree) acc' is
    Text s -> f idx (Text s) acc
  Nothing -> acc
