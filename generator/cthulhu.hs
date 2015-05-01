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
import CmdPrinter
import ErrM


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


run :: [String] -> IO ()
run file = do
  prog <- parseFiles ("cthulhu_lib/lib.ct":file)
  t <- typecheck prog
  case t of
    Right x -> do
      r <- generate x
      out_file <- openFile "runtimes/seq/gen.h" WriteMode 
      print_cmds r out_file
      hClose out_file
      runCmd "cd runtimes/seq; make"
      runCmd "cp runtimes/seq/run ."
    Left err -> do
      putStrLn "Typecheck failed:"
      putStrLn err

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "Filename expected"
            fs -> run fs





