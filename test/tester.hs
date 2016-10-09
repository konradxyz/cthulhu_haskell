import System.IO
import System.Directory
import Data.String.Utils
import System.Environment
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

test_case :: (String -> String) -> String -> String -> IO()
test_case compilation exec name = do
  putStrLn $ "Case " ++ name ++ ":"
  runCmd $ compilation name
  let case_name = takeWhile (/= '.') name in do
    inputs <- readFile $ cases_dir ++ "/" ++ case_name ++ ".in"
    outputs <- readFile $ cases_dir ++ "/" ++ case_name ++ ".out"
    mapM_ (uncurry $ test_exec exec) $ zip (lines inputs) (lines outputs) 
    runCmd "rm run"
    return ()

compilation :: String -> String -> String -> String
compilation ext opts name = "./cthulhu " ++  " " ++ cases_dir ++ "/" ++ name ++ "." ++ ext ++ " " ++ opts

files_with_ext :: String -> IO [String]
files_with_ext extension = do
  args <- getArgs
  files <- case args of
    [] -> getDirectoryContents cases_dir
    _ -> return args
  return $ map (\x -> take (length x - length  ("." ++ extension)) x) $ filter (endswith  ("." ++ extension)) files
  

main = do
  cases <- files_with_ext "ct"
  mapM (test_case (compilation "ct" "--par --move-opt --light-queue --inline") "--threads 4") $ cases
  mapM (test_case (compilation "ct" "--par --move-opt --light-queue") "--threads 4") $ cases
  mapM (test_case (compilation "ct" "--tco --par --move-opt") "--threads 4") $ cases
  mapM (test_case (compilation "ct" "") "") $ cases
  mapM (test_case (compilation "ct" "--par") "--threads 4") $ cases
  mapM (test_case (compilation "ct" "--par --move-opt") "--threads 4") $ cases
  mapM (test_case (compilation "ct" "--tco --par") "--threads 4") $ cases
  cases <- files_with_ext "par.casm"
  mapM (test_case (compilation "par.casm" "--par --icasm") "--threads 4") $ cases
  putStrLn "All checks succeeded"
