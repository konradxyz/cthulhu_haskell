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

show_arithop :: ArithOp -> String
show_arithop (Const n) = show n
show_arithop (Local n) = "ENV_INT(" ++ show n ++ ")"
show_arithop (Operation op l r) = show_optype op ++ "(" ++ show_arithop l ++ ", " 
	++  show_arithop r  ++ ")"

print_cmd :: Handle -> Map.Map Int FunctionCall -> Cmd -> IO()
print_cmd h _ (Load id) = hPutStrLn h $ "LOAD_COPY(" ++ show id ++ ")"
print_cmd h _ (AllocParams c) = hPutStrLn h $ "ALLOC_PARAMS(" ++ show c ++ ")"
print_cmd h f (AllocFunctionEnv id) = do
  f <- get_function f id
  hPutStrLn h $ "ALLOC_PARAMS(" ++ show (env_size f) ++ ")" 
print_cmd h _ (PrepareParamMove from to) =
	hPutStrLn h $ "PREPARE_PARAM_MOVE(" ++ show from ++ ", " ++ show to ++ ")"
print_cmd h _ (JmpIfZero lab) =
	hPutStrLn h $ "JMP_IF_ZERO(" ++ show lab ++ ")"
print_cmd h _ (Jmp lab) =
	hPutStrLn h $ "JMP(" ++ show lab ++ ")"
print_cmd h _ (AddParamMove id) = 
	ioError $ userError "Printing AddParamMove - should not happen"
print_cmd h _ (AddParamMoveWithLabel id lab) = 
	hPutStrLn h $ "ADD_PARAM_MOVE(" ++ show id ++ ", " ++ show lab ++ ")"
print_cmd h _ (Arith op) =
	hPutStrLn h $ "ARITH(" ++ show_arithop op ++ ")"
print_cmd h _ (StoreArith id) = hPutStrLn h $ "STORE_ARITH(" ++ show id ++ ")" 
print_cmd h _ LoadArith = hPutStrLn h $ "LOAD_ARITH" 
print_cmd h _ ArithLoadAcc = hPutStrLn h $ "ARITH_LOAD_ACC" 
print_cmd h _ (Construct id) = hPutStrLn h $ "CONSTRUCT(" ++ show id ++ ")"
print_cmd h _ (Store id) = hPutStrLn h $ "STORE(" ++ show id ++ ")"
print_cmd h f (Call id) = ioError $ userError "Call printed - should not happen" 
print_cmd h f (CallWithLabel id label) = do
	f <- get_function f id
	hPutStrLn h $ "CALL(" ++ show (flabel f)  ++ ", " ++ show label ++ ")"
print_cmd h f (Global id) = do
	f <- get_function f id
	hPutStrLn h $ "GLOBAL(" ++ show (flabel f) ++ ", " ++ show (params f) ++ ", " 
		++ show (env_size f) ++  ")"
print_cmd h _ (JmpCase c) = let targets = intercalate " COMMA " $ map show c in do
	hPutStrLn h $ "JMP_CASE({" ++ targets ++ "})" 
print_cmd h _ Ret = hPutStrLn h $ "RET" 
print_cmd h _ Skip = hPutStrLn h $ "SKIP" 
print_cmd h _ (StoreField from to) = hPutStrLn h $ "STORE_FIELD(" ++ show from 
	++ "," ++ show to ++ ")"
print_cmd h _ Finalize = hPutStrLn h $ "FINALIZE" 

print_label :: Handle -> Label -> IO()
print_label h l = case l of
  Label id -> hPutStrLn h $ "case " ++  show id ++ ":"
  NamedLabel id comment -> hPutStrLn h $ "case " ++  show id++ ": /*" ++ comment ++ "*/"


print_cmdseq :: Handle -> Map.Map Int FunctionCall -> CmdSeq -> IO()
print_cmdseq h fc c = do
	case label c of
		Just lab -> print_label h lab
		Nothing -> return ()
	print_cmd h fc (cmd c)

print_entry :: FunctionCall -> Int -> Handle -> IO()
print_entry main end h = do
	mapM_ (hPutStrLn h) $ [
		"#ifndef CTHULHU_GEN_H_",
		"#define CTHULHU_GEN_H_",
		"#include \"static/seq/seq.h\"",
		"#define START_ENV_SIZE " ++ (show $ env_size main), 
		"#define START_LABEL " ++ (show $ flabel main),
		"#define FINAL_LABEL " ++ (show $ end),
		"void executeContext(seq::Context* context) {",
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

print_cmds :: Cmds -> Handle -> IO()
print_cmds (Cmds cmds fun main end) handle  = do
	print_entry main end handle
	mapM_ (print_cmdseq handle fun) cmds
	print_leave handle
