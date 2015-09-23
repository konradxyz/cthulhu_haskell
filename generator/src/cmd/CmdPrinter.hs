module CmdPrinter where


import Cmd
import System.IO
import qualified Data.Map as Map
import Data.List

get_function :: Map.Map Int FunctionCall -> Int -> IO FunctionCall
get_function m id = case Map.lookup id m of
	Just x -> return x
	Nothing ->  ioError $ userError "Unknown function - should not happen" 

show_optype :: OpType -> String
show_optype Add = "ADD"
show_optype Sub = "SUB"
show_optype Lt = "LT"
show_optype Le = "LE"
show_optype Gt = "GT"
show_optype Ge = "GE"
show_optype Eq = "EQ"
show_optype Neq = "NEQ"
show_optype And = "AND"
show_optype Or = "OR"
show_optype Mul = "MUL"

show_arithop :: ArithOp -> String
show_arithop (Const n) = show n
show_arithop (Local n) = "ENV_INT(" ++ show n ++ ")"
show_arithop (Operation op l r) = show_optype op ++ "(" ++ show_arithop l ++ ", " 
	++  show_arithop r  ++ ")"

err_cmd cmd = ioError $ userError $ "Printing " ++ cmd ++ "- should not happen"

print_cmd :: Map.Map Int FunctionCall -> Cmd -> IO String
print_cmd _ (Load id) = return $ "LOAD_COPY(" ++ show id ++ ")"
print_cmd _ (LoadMove id) = return $ "LOAD_MOVE(" ++ show id ++ ")"
print_cmd _ (AllocParams c) = return $ "ALLOC_PARAMS(" ++ show c ++ ")"
print_cmd f (AllocFunctionEnv id) = do
  f <- get_function f id
  return $ "ALLOC_PARAMS(" ++ show (env_size f) ++ ")" 
print_cmd _ (PrepareParamMove from to) =
	return $ "PREPARE_PARAM_MOVE(" ++ show from ++ ", " ++ show to ++ ")"
print_cmd _ (JmpIfZero lab) =
	return $ "JMP_IF_ZERO(" ++ show lab ++ ")"
print_cmd _ (Jmp lab) =
	return $ "JMP(" ++ show lab ++ ")"
print_cmd _ (AddParamMove id) = err_cmd "AddParamMove" 
print_cmd _ (AddParamMoveParFork id) = err_cmd "AddParamMoveParFork" 
print_cmd _ (AddParamMoveWithLabel id lab) = 
	return $ "ADD_PARAM_MOVE(" ++ show id ++ ", " ++ show lab ++ ")"
print_cmd _ (AddParamMoveParForkWithLabel id lab) =
	return $ "ADD_PARAM_MOVE_FORK(" ++ show id ++ ", " ++ show lab ++ ")"
print_cmd _ (Wait id) = err_cmd "Wait" 
print_cmd _ (WaitWithLabel id lab) = 
	return $ "WAIT(" ++ show id ++ ", " ++ show lab ++ ")"
print_cmd _ (Arith op) =
	return $ "ARITH(" ++ show_arithop op ++ ")"
print_cmd _ (StoreArith id) = return $ "STORE_ARITH(" ++ show id ++ ")" 
print_cmd _ LoadArith = return $ "LOAD_ARITH" 
print_cmd _ ArithLoadAcc = return $ "ARITH_LOAD_ACC" 
print_cmd _ (Construct id) = return $ "CONSTRUCT(" ++ show id ++ ")"
print_cmd _ (Store id) = return $ "STORE(" ++ show id ++ ")"
print_cmd f (Call id) = err_cmd "Call"
print_cmd f (CallWithLabel id label) = do
	f <- get_function f id
	return $ "CALL(" ++ show (flabel f)  ++ ", " ++ show label ++ ")"
print_cmd f (CallFork id) = err_cmd "CallFork"
print_cmd f (CallForkWithLabel id label) = do
	f <- get_function f id
	return $ "CALL_FORK(" ++ show (flabel f)  ++ ", " ++ show label ++ ")"
print_cmd f (Global id) = do
	f <- get_function f id
	return $ "GLOBAL(" ++ show (flabel f) ++ ", " ++ show (params f) ++ ", " 
		++ show (env_size f) ++  ")"
print_cmd f (GlobalPar id) = do
  f <- get_function f id
  if Cmd.is_complex f
    then return $ "GLOBAL_FORK(" ++ show (flabel f) ++ ", " ++ show (params f) ++ ", " 
           ++ show (env_size f) ++  ")"
    else return $ "GLOBAL(" ++ show (flabel f) ++ ", " ++ show (params f) ++ ", " 
           ++ show (env_size f) ++  ")"
print_cmd _ (JmpCase c) = let targets = intercalate " COMMA " $ map show c in do
  return $ "JMP_CASE({" ++ targets ++ "})" 
print_cmd _ Ret = return "RET" 
print_cmd _ Skip = return "SKIP" 
print_cmd _ (StoreField from to) = return $ "STORE_FIELD(" ++ show from ++ "," ++ show to ++ ")"
print_cmd _ Finalize = return "FINALIZE" 
print_cmd f (CallTail id) = do
  f <- get_function f id
  return $ "CALL_TAIL(" ++ show (flabel f) ++ ")"
print_label :: Handle -> Int -> IO()
print_label h l = hPutStrLn h $ "case " ++  show l ++ ":"


print_cmdseq :: Handle -> Map.Map Int FunctionCall -> CmdSeq -> IO()
print_cmdseq h fc c = do
  case label c of
    Just lab -> print_label h lab
    Nothing -> return ()
  cmd_print <- print_cmd fc (cmd c)
  comment_print <- case comment c of
    Just comment -> return $ "// " ++ comment
    Nothing -> return ""
  hPutStrLn h (cmd_print ++ comment_print)

print_entry :: Bool -> FunctionCall -> Int -> Handle -> IO()
print_entry unique main end h = 
  let ctx_type = if unique then "std::unique_ptr<seq::Context>&&" else "seq::Context*" in
  	mapM_ (hPutStrLn h) $ [
	  	"#ifndef CTHULHU_GEN_H_",
		  "#define CTHULHU_GEN_H_",
	  	"#define START_ENV_SIZE " ++ (show $ env_size main), 
		  "#define START_LABEL " ++ (show $ flabel main),
  		"#define FINAL_LABEL " ++ (show $ end),
  		"#include \"static/seq/seq.h\"",
	  	"void executeContext(" ++ ctx_type ++ " context) {",
		  "while ( context != nullptr ) {",
  		"switch (context->nextInstruction) {"
	  	]

print_leave :: Handle -> IO()
print_leave h = mapM_ (hPutStrLn h) [
		"default:",
		"break;",
		"}", 
		"}", 
		"}",
		"#endif"
		]

print_cmds :: Bool -> Cmds -> Handle -> IO()
print_cmds unique (Cmds cmds fun main end) handle  = do
	print_entry unique main end handle
	mapM_ (print_cmdseq handle fun) cmds
	print_leave handle
