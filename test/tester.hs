import System.IO
import System.Directory
import Data.String.Utils
import System.Process

import System.Exit
import GHC.IO.Handle


cases_dir = "test/cases"

runCmd :: String -> IO String
runCmd cmd = do
  putStrLn $ "Running " ++ cmd
  (_, out, err, handle) <- runInteractiveCommand cmd
  sout <- hGetContents out
  serr <- hGetContents err
  code <- waitForProcess handle
  case code of
    ExitSuccess -> do
      putStrLn "Command succeeded"
      return sout
    _ -> do 
      putStrLn $ "Failed: " ++ cmd 
      putStrLn "STDOUT"
      putStrLn sout
      putStrLn "STDERR"
      putStrLn serr
      exitWith code

test_exec :: String -> String -> String -> IO()
test_exec exec input expected = do
  output <- runCmd $ "./run "++ input ++ " " ++ exec
  output <- return .  rstrip $ output
  if output /= expected then do
    putStrLn $ "Failure: param " ++ input ++ ", expected " ++ expected ++ ", received " ++ output
    exitWith $ ExitFailure 1
  else
    return ()

test_case :: String -> String -> String ->  IO()
test_case compilation exec name = do
  putStrLn $ "Case " ++ name ++ ":"
  runCmd $ "./cthulhu " ++  " " ++ cases_dir ++ "/" ++ name ++ ".ct " ++ compilation
  inputs <- readFile $ cases_dir ++ "/" ++ name ++ ".in"
  outputs <- readFile $ cases_dir ++ "/" ++ name ++ ".out"
  mapM_ (uncurry $ test_exec exec) $ zip (lines inputs) (lines outputs) 

main = do
  files <- getDirectoryContents(cases_dir)
  let cases = map (\x -> take (length x - length ".ct") x) $ filter (endswith ".ct") files in do
    mapM (test_case "" "") $ cases
    mapM (test_case "--par" "4") $ cases
  putStrLn "All checks succeeded"
