import System.Environment 



fibo n = if n < 2 then n else fibo (n - 1) + fibo (n - 2)

main = do
  (h:_) <- getArgs
  putStrLn $ show $ fibo $ (read::String->Int) h
  