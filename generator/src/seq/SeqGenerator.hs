module SeqGenerator where


import Ast
import Cmd
import qualified Data.Map as Map
import Control.Monad.State
import CmdGeneratorUtils
import System.IO

handle_int :: Exp -> NeedType -> FunctionGenerator [CmdSeq]
handle_int e nt = do
    (bin, other, _) <- gather_binary e 
    res <- mapM (\te -> generate_exp (texp te) (ToEnv $ ttarget te)) other
    need <- arith_need nt
    return $ concat res ++ [no_label $ Arith bin] ++ need


handle_params :: [Exp] -> Cmd -> Cmd -> NeedType -> FunctionGenerator [CmdSeq]
handle_params exps prep finalize nt = do
    params <- mapM (\e -> alloc_local >>= \id -> return $ TargetedExp e id) exps
    res <- mapM (\te -> generate_exp (texp te) (ToEnv $ ttarget te)) params
    let params_move = map (\e -> no_label $ PrepareParamMove (ttarget $ fst e) $ snd e) $ zip params [0..] in do
        fin <- acc_need nt
        return $ concat res ++ 
          ([no_label prep] ++ params_move ++ [no_label finalize]) ++ fin

generate_case :: CaseVariant -> NeedType -> Int -> Int -> FunctionGenerator [CmdSeq]
generate_case (CaseVariant f e) nt ml el = do
  let fields_move = map (\(fid, target) -> no_label $ StoreField fid target) $ zip [0..] f in do
    ce <- generate_exp e nt
    return $ [CmdSeq (Just $ Label ml) Skip] ++ fields_move ++ ce ++ [no_label $ Jmp el]
    

generate_exp :: Exp -> NeedType -> FunctionGenerator [CmdSeq]
generate_exp e nt = case e of
  Ast.Construct id exps -> handle_params exps (AllocParams $ length exps) (Cmd.Construct id) nt
  Operator _ _ _ -> handle_int e nt
  Ast.Apply f p -> do
    id <- alloc_local
    pe <- generate_exp p (ToEnv id)
    fe <- generate_exp f Acc
    fin <- acc_need nt
    return $ pe ++ fe ++ [no_label $ AddParamMove id] ++ fin
  Ast.Const _ -> handle_int e nt 
  Ast.Local id -> do
    fin <- acc_need nt
    return $ [no_label $ Load id] ++ fin
  Ast.Global id p exps -> handle_params exps (AllocParams $ length exps) (Cmd.Global id) nt  
  Ast.Call id exps ->  handle_params exps (AllocFunctionEnv id) (Cmd.Call id) nt  
  Ast.Let id val ret -> do
    cval <- generate_exp val $ ToEnv id
    cret <- generate_exp ret nt
    return $ cval ++ cret
  Ast.If c t f -> do
    cc <- generate_exp c ArithNeed
    ct <- generate_exp t nt
    lf <- alloc_label
    cf <- generate_exp f nt
    le <- alloc_label
    return $ cc ++ [no_label $ JmpIfZero lf] ++ ct ++ [no_label $ Jmp le]
       ++ [CmdSeq (Just $ Label lf) Skip] ++  cf ++ [CmdSeq (Just $ Label le) Skip]
  Ast.Case value cases -> do
    cval <- generate_exp value $ Acc
    end_label <- alloc_label
    lcases <- mapM (\x -> alloc_label >>= \l -> return (x, l)) cases
    ccases <- mapM (\(mc, ml) -> generate_case mc nt ml end_label) lcases 
    return $ cval ++ [no_label $ JmpCase $ map snd lcases] ++ concat ccases 
      ++ [CmdSeq (Just $ Label end_label) Skip] 
    


generate_function :: Ast.FunctionSpec -> Ast.Function -> SeqGenerator ([CmdSeq], FunctionCall)
generate_function spec (Ast.Function fid params locals exp) = do
  flable <- s_alloc_label
  ce <- runStateT (generate_exp exp Return) $ FunctionGeneratorState locals
  ace <- assign_apply_labels $ fst ce
  return ((CmdSeq (Just $ NamedLabel flable $ Ast.fname spec) Skip:ace), 
    FunctionCall fid flable params (next_local $ snd ce) (Ast.fname spec) False)

generate_program :: Program -> SeqGenerator Cmds
generate_program prog = do
  cmds <- mapM (\(k, v) -> generate_function k v) $ Map.toList $ Ast.functions prog
  let calls =  Map.fromList $ map (\(_, x) -> (Cmd.fid x, x)) cmds in do
    final_label <- s_alloc_label
    case Map.lookup 0 calls of
      Just x -> return $ Cmds ((concat $ map fst cmds) ++ [CmdSeq (Just $ Label final_label) 
        Finalize]) calls x final_label
      Nothing -> sio $ ioError $ userError "Unknown function - should not happen" 

generate :: Program -> IO Cmds
generate p = do
  evalStateT (generate_program p) (SeqGeneratorState 0 (\x-> False))



