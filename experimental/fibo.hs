import System.Environment 



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

queens :: Int -> Int -> [Int] -> Int
queens current max prev = if current < max then
    foldl (+) 0 $ map (queens (current + 1) max ) $ map (:prev) [0..max - 1]
  else if exists_queen_seen 0 prev then 0 else 1



fun n = queens 0 n []

main = do
  (h:_) <- getArgs
  putStrLn $ show $ fun $ (read::String->Int) h
  
