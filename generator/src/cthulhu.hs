module Main where


import System.IO
import System.Exit
import System.Environment ( getArgs, getProgName )
import System.Process
import GHC.IO.Handle

import Lexcthulhu
import Parcthulhu
import Skelcthulhu
import Printcthulhu
import Abscthulhu

import Ast
import Typechecker
import Template
import SeqGenerator
import ParGenerator
import CmdPrinter
import ErrM
import Utils

parser = pProgram


parseFile :: String -> IO(Abscthulhu.Program)
parseFile fname = do
  input <- readFile fname
  case parser $ myLexer input of
    Bad s -> do
      putStrLn $ "Could not parse " ++ fname
      putStrLn s
      exitFailure
    Ok tree -> return tree 

parseFiles :: [String] -> IO(Abscthulhu.Program)
parseFiles files = do
  progs <- mapM parseFile files
  return $ Abscthulhu.RealProgram $ concat $ 
    map (\x -> let Abscthulhu.RealProgram p = x in p) progs 

runCmd :: String -> IO()
runCmd cmd = do
  putStrLn $ "Running " ++ cmd
  (_, out, err, handle) <- runInteractiveCommand cmd
  sout <- hGetContents out
  serr <- hGetContents err
  code <- waitForProcess handle
  case code of
    ExitSuccess -> do
      putStrLn "Command succeeded"
      return()
    _ -> do 
      putStrLn $ "Failed: " ++ cmd 
      putStrLn "STDOUT"
      putStrLn sout
      putStrLn "STDERR"
      putStrLn serr
      exitWith code


run :: Options -> [String] -> IO ()
run opts file = do
  prog <- parseFiles ("cthulhu_lib/lib.ct":file)
  t <- typecheck prog
  case t of
    Right x -> do
      if par opts
        then do
          r <- ParGenerator.generate x
          out_file <- openFile "runtimes/par/gen.h" WriteMode 
          print_cmds True r out_file
          hClose out_file
          return ()
        else do
          r <- SeqGenerator.generate x
          out_file <- openFile "runtimes/seq/gen.h" WriteMode 
          print_cmds False r out_file
          hClose out_file
          runCmd "cd runtimes/seq; make"
          runCmd "cp runtimes/seq/run ."
          if icpc opts
            then runCmd "scripts/prepare_icpc.sh"
            else return ()
    Left err -> do
      putStrLn "Typecheck failed:"
      putStrLn err

data Options = Options {
  icpc :: Bool,
  par :: Bool
} deriving (Show)


separate :: [String] -> ([String], [String])
separate [] = ([], [])
separate (h:t) = let (par, opts) = separate t in
  if starts_with "--" h
    then (par, (drop 2 h:opts))
    else ((h:par), opts)

options :: [String] -> IO Options
options opts = 
  let (r1, icpc) = check_remove "icpc" opts in
    let (r, par) = check_remove "par" r1 in
      case r of
        [] -> return $ Options icpc par
        (h:_) -> do
          putStrLn $ "Unknown option " ++ h
          exitFailure

main :: IO ()
main = do 
  arg <- getArgs
  let (args, opts) = separate arg in do
    opts <- options opts
    print opts
    case args of
      [] -> putStrLn "Filename expected"
      fs -> run opts fs





