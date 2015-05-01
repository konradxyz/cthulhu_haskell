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

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> ParseFun Abscthulhu.Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

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
               

run :: Verbosity -> ParseFun Abscthulhu.Program -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree 
                          t <- typecheck tree
                          case t of
                            Right x -> do
                              r <- generate x
                              print r
                              out_file <- openFile "runtimes/seq/gen.h" WriteMode 
                              print_cmds r out_file
                              hClose out_file
                              runCmd "cd runtimes/seq; make"
                            Left err -> do
                              putStrLn "Typecheck failed:"
                              putStrLn err



showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= run 2 pProgram
            "-s":fs -> mapM_ (runFile 0 pProgram) fs
            fs -> mapM_ (runFile 2 pProgram) fs





