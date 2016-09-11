import System.Environment 
import Control.Parallel
import Control.Parallel.Strategies


--fibo n = if n < 2 then n else fibo (n - 1) + fibo (n - 2)

rooks :: Int -> Int -> [Int] -> Int
rooks current max prev = if current < max then
    foldl (+) 0 $ map (rooks (current + 1) max ) $ map (:prev) [0..max - 1]
  else 1


or = (||)
add = (+)
eq = (==)
sub = (-)

queens_see_each_other :: Int -> Int -> Int -> Int -> Bool
queens_see_each_other xrow xcol yrow ycol = 
    Main.or 
      (Main.or (Main.or (eq xrow yrow) (eq xcol ycol))  (eq (sub xrow yrow) (sub xcol ycol)))
      (eq 0 (add (sub xrow yrow) (sub xcol ycol)))

queen_is_seen_by_any :: Int -> Int -> Int -> [Int] -> Bool
queen_is_seen_by_any xrow xcol yrow ycols = case ycols of
  [] -> False
  (ycol: ytail) -> 
    if queens_see_each_other xrow xcol yrow ycol 
      then True
      else queen_is_seen_by_any xrow xcol (add yrow 1) ytail


exists_queen_seen :: Int -> [Int] -> Bool
exists_queen_seen xrow cols = case cols of
  [] -> False
  (xcol : ycols) -> 
    if queen_is_seen_by_any xrow xcol (add xrow 1) ycols 
      then True
      else exists_queen_seen (add xrow 1) ycols

queens :: Int ->  Int -> Int -> [Int] -> Int
queens r current max prev = if current < max then
    foldl (+) 0 $ map (queens r (current + 1) max ) $ map (:prev) [0..max - 1]
  else if exists_queen_seen 0 prev then 0 else r

_queens :: Int -> Int -> Int -> [Int] -> Int
_queens x c m p = x + queens x c m p
mmap :: (a -> b) -> [a] -> [b]
mmap f l = case l of
  [] -> []
  (a:t) -> (f a: mmap f t)

mmap_par :: (a -> b) -> [a] -> [b]
mmap_par f l = case l of
  [] -> []
  (a:t) -> runEval $ do
    fa <- rpar $ f a
    ft <- rseq $ mmap_par f t
    return (fa : ft)


--  let x = f a in x `rpar` (x: mmap f t)
numbers1 = 1 : map (+1) numbers1
--mmap_par_pr = parMap rdeepseq
fun n = mmap_par  (\x -> _queens x 0 n []) $ take 16 numbers1
main = do
  (h:_) <- getArgs
  putStrLn $ show $ fun $ (read::String->Int) h
  
