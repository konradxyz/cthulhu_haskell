module Main where


import System.IO
import System.Console.ArgParser
import System.Directory
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


run :: Options -> IO ()
run opts = do
  (runtime, generator) <- if par opts then
      return ("par", ParGenerator.generate)
    else
      return ("seq", SeqGenerator.generate)
  if icasm opts then do
    runCmd $ "cp " ++ file opts ++ " runtimes/" ++ runtime ++ "/gen.h"
  else do
    prog <- parseFiles ["cthulhu_lib/lib.ct", file opts]
    t <- typecheck prog
    case t of
      Right x -> do
        r <- generator x
        out_file <- openFile ("runtimes/" ++ runtime ++ "/gen.h") WriteMode 
        print_cmds (par opts) r out_file
        hClose out_file
      Left err -> do
        putStrLn "Typecheck failed:"
        putStrLn err
        exitWith $ ExitFailure 1
  runCmd $ "cd runtimes/" ++ runtime ++ "; make"
  runCmd $ "cp runtimes/" ++ runtime ++ "/run ."
  if icpc opts
    then runCmd $ "scripts/" ++ runtime ++ "_prepare_icpc.sh"
    else return ()
  if casm opts
    then runCmd $ "cp runtimes/" ++ runtime ++ "/gen.h run.casm"
    else return ()
 
data Options = Options {
  file :: String,
  icpc :: Bool,
  par :: Bool,
  casm :: Bool,
  icasm :: Bool
} deriving (Show)

options_parser :: ParserSpec Options
options_parser = Options 
  `parsedBy` reqPos "file"
  `andBy` (boolFlag "icpc")
  `andBy` (boolFlag "par")
  `andBy` (boolFlag "casm")
  `andBy` (boolFlag "icasm")

main :: IO ()
main = withParseResult options_parser run
