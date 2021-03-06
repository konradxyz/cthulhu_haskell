module Main where


import System.IO
import System.Console.ArgParser
import System.Directory
import System.Exit
import System.Environment ( getArgs, getProgName )
import System.Process
import GHC.IO.Handle

import qualified AbsCthulhu as Abs
import qualified ParCthulhu as Par

import Ast
import qualified Config
import qualified Inliner
import Typechecker
import Template
import SeqGenerator
import ParGenerator
import CmdPrinter
import ErrM
import Utils

parser = Par.pProgram

parseFile :: String -> IO(Abs.Program)
parseFile fname = do
  input <- readFile fname
  case parser $ Par.myLexer input of
    Bad s -> do
      putStrLn $ "Could not parse " ++ fname
      putStrLn s
      exitFailure
    Ok tree -> return tree 

parseFiles :: [String] -> IO(Abs.Program)
parseFiles files = do
  progs <- mapM parseFile files
  return $ Abs.RealProgram $ concat $ 
    map (\x -> let Abs.RealProgram p = x in p) progs 

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

ffail :: String -> IO()
ffail msg = do
  putStrLn msg
  exitWith $ ExitFailure 1

generate_config :: Options -> IO Config.Config
generate_config opts = let res = Config.Config (par opts) (tco opts) (move_opt opts) in do
  if Config.tco res && not (Config.par res) then
    ffail "TCO not yet supported in seq"
  else
    return ()
  return res

run :: Options -> IO ()
run opts = do
  conf <- generate_config opts
  (runtime, generator) <- if par opts then do
      return ("par", ParGenerator.generate)
    else
      return ("seq", SeqGenerator.generate)
  inliner <- if inline opts then do
      return Inliner.inline
    else
      return id
  if icasm opts then do
    runCmd $ "cp " ++ file opts ++ " runtimes/" ++ runtime ++ "/gen.h"
  else do
    prog <- parseFiles ["cthulhu_lib/lib.ct", file opts]
    t <- typecheck prog
    case t of
      Right x -> do
        r <- generator conf x
        out_file <- openFile ("runtimes/" ++ runtime ++ "/gen.h") WriteMode 
        print_cmds (par opts) r out_file
        hClose out_file
      Left err -> do
        putStrLn "Typecheck failed:"
        putStrLn err
        exitWith $ ExitFailure 1
  custom_opts <- if light_queue opts then
      return "-DNO_MUTEX"
  else
      return ""
  runCmd $ "bash -c 'sed s/__CUSTOM_OPTS__/" ++ custom_opts ++ "/ runtimes/" ++ runtime ++ "/Makefile.template > runtimes/" ++ runtime ++ "/Makefile'"
  runCmd $ "cd runtimes/" ++ runtime ++ "; make clean; make"
  runCmd $ "cp runtimes/" ++ runtime ++ "/run ."
  if icpc opts
    then runCmd $ "scripts/" ++ runtime ++ "_prepare_icpc.sh"
    else return ()
  if casm opts
    then runCmd $ "cp runtimes/" ++ runtime ++ "/gen.h run.casm"
    else return ()
  runCmd $ "rm runtimes/" ++ runtime ++ "/Makefile"
 
data Options = Options {
  file :: String,
  icpc :: Bool,
  par :: Bool,
  casm :: Bool,
  icasm :: Bool,
  tco :: Bool,
  move_opt :: Bool,
  light_queue :: Bool,
  inline :: Bool
} deriving (Show)

options_parser :: ParserSpec Options
options_parser = Options 
  `parsedBy` reqPos "file"
  `andBy` (boolFlag "icpc")
  `andBy` (boolFlag "par")
  `andBy` (boolFlag "casm")
  `andBy` (boolFlag "icasm")
  `andBy` (boolFlag "tco")
  `andBy` (boolFlag "move-opt")
  `andBy` (boolFlag "light-queue")
  `andBy` (boolFlag "inline")

main :: IO ()
main = withParseResult options_parser run
