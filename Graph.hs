module Graph
  ( addEdge
  , addVertex
  , empty
  , fromList
  , neighbors
  , Graph
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

data Graph a = Graph { vertices :: S.Set a
                     , edges :: M.Map a (S.Set a)
                     } deriving (Show)

addEdge :: (Ord a) => Graph a -> (a, a) -> Graph a
addEdge (Graph {vertices=vs, edges=es}) (v0, v1) =
  case M.lookup v0 es of
    Nothing -> Graph { vertices = new_vs
                     , edges = M.insert v0 (S.singleton v1) es
                     }
    Just dsts -> Graph { vertices = new_vs
                       , edges = M.insert v0 (S.insert v1 dsts) es
                       }
  where new_vs = S.union vs $ S.fromList [v0, v1]

addVertex :: (Ord a) => Graph a -> a -> Graph a
addVertex (Graph {vertices=vs, edges=es}) v =
  Graph { vertices=S.insert v vs
        , edges=es
        }

empty :: Graph a
empty = Graph { vertices=S.empty
              , edges=M.empty
              }

fromList :: (Ord a) => [(a, a)] -> Graph a
fromList es = foldl addEdge empty es

neighbors :: (Ord a) => Graph a -> a -> S.Set a
neighbors Graph { edges = es } v =
  case M.lookup v es of
    Nothing -> S.empty
    Just dsts -> dsts
