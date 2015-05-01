import System.Environment 



--fibo n = if n < 2 then n else fibo (n - 1) + fibo (n - 2)

rooks :: Int -> Int -> [Int] -> Int
rooks current max prev = if current < max then
    foldl (+) 0 $ map (rooks (current + 1) max ) $ map (:prev) [0..max - 1]
  else 1

fun n = rooks 0 n []

main = do
  (h:_) <- getArgs
  putStrLn $ show $ fun $ (read::String->Int) h
  
