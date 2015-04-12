module Utils where

import qualified Data.Map as Map

unique :: Ord k => (a -> k) -> [a] -> Either (k, a, a)  (Map.Map k a)
unique key ls = 
  let pairs = zip (map key ls) ls in
    foldl f (Right Map.empty) pairs
    where f m e = case m of
                  Left e -> Left e
                  Right mp -> case Map.lookup (fst e) mp of
                    Just v -> Left (fst e, snd e, v)
                    Nothing -> Right $  Map.insert (fst e) (snd e) mp



