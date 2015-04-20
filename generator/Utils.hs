module Utils where

import Data.List
import Data.Maybe
import qualified Data.Map as Map

unique :: Ord k => (a -> k) -> [a] -> Either (k, a, a)  (Map.Map k a)
unique key ls = unique_v key (id::a->a) ls 


unique_v :: Ord k => (a -> k) -> (a->b) -> [a] -> Either (k, b, b)  (Map.Map k b)
unique_v key val ls = 
  let pairs = zip (map key ls) (map val ls) in
    foldl f (Right Map.empty) pairs
    where f m e = case m of
                  Left e -> Left e
                  Right mp -> case Map.lookup (fst e) mp of
                    Just v -> Left (fst e, snd e, v)
                    Nothing -> Right $  Map.insert (fst e) (snd e) mp

exists :: Ord k => [k] -> Map.Map k a -> Maybe k
exists keys m = case intersect keys (Map.keys m) of
  (h:_) -> Just h
  [] -> Nothing
