module SeqGenerator where


import Ast
import Cmd
import qualified Data.Map as Map
import Control.Monad.State

import System.IO


data SeqGeneratorState = SeqGeneratorState {
  next_label :: Int
} deriving (Show)

type SeqGenerator = StateT SeqGeneratorState IO


data FunctionGeneratorState = FunctionGeneratorState {
  next_local :: Int
} deriving (Show)

type FunctionGenerator = StateT FunctionGeneratorState SeqGenerator

alloc_local :: FunctionGenerator Int
alloc_local = do
  ret <- gets next_local
  state <- get
  put $ state { next_local = 1 + next_local state }
  return $ next_local state

s_alloc_label :: SeqGenerator Int
s_alloc_label = do
  state <- get
  put $ state { next_label = 1 + next_label state }
  return $ next_label state




alloc_label :: FunctionGenerator Int
alloc_label = lift s_alloc_label

op_ast_to_cmd :: Ast.OpType -> Cmd.OpType
op_ast_to_cmd Ast.Add = Cmd.Add
op_ast_to_cmd Ast.Sub = Cmd.Sub
op_ast_to_cmd Ast.Lt = Cmd.Lt
op_ast_to_cmd Ast.Le = Cmd.Le
op_ast_to_cmd Ast.Gt = Cmd.Gt
op_ast_to_cmd Ast.Ge = Cmd.Ge
op_ast_to_cmd Ast.Eq = Cmd.Eq
op_ast_to_cmd Ast.Neq = Cmd.Neq
op_ast_to_cmd Ast.And = Cmd.And
op_ast_to_cmd Ast.Or = Cmd.Or
op_ast_to_cmd Ast.Mul = Cmd.Mul




data TargetedExp = TargetedExp {
  texp :: Exp,
  ttarget :: Int
}



gather_binary :: Exp -> FunctionGenerator (ArithOp, [TargetedExp])
gather_binary e = case e of
  Operator op l r -> do
    (la, lt) <- gather_binary l
    (ra, rt) <- gather_binary r
    return (Operation (op_ast_to_cmd op) la ra, lt ++ rt)
  Ast.Const n -> return $ (Cmd.Const n, [])
  Ast.Local id -> return $ (Cmd.Local id, [])
  _ -> do
    id <- alloc_local
    return $ (Cmd.Local id, [TargetedExp e id])
 
data NeedType =
  ToEnv Int | Return | Acc | ArithNeed

arith_need :: NeedType -> FunctionGenerator [CmdSeq]
arith_need (ToEnv id) = return $ m_nl [StoreArith id]
arith_need Acc = return $ m_nl [LoadArith]
arith_need Return = return $ m_nl [LoadArith, Ret]
arith_need ArithNeed = return $ m_nl []

acc_need :: NeedType -> FunctionGenerator [CmdSeq]
acc_need (ToEnv id) = return $ m_nl [Store id]
acc_need Acc = return []
acc_need Return = return $ m_nl [Ret]
acc_need ArithNeed = return $ m_nl [ArithLoadAcc] 


no_label :: Cmd -> CmdSeq
no_label c = CmdSeq Nothing c
m_nl = map no_label
{-

data Exp = 
  Construct Int [Exp] |
  Operator OpType Exp Exp | 
  Apply Exp Exp |
  Const Integer |
  Local Int |
  Global Int [Exp] |
  Call Int [Exp] |
  Let Int Exp Exp |
  If Exp Exp Exp |
  Case Exp [CaseVariant]
  deriving (Eq, Show, Ord)

-}

handle_int :: Exp -> NeedType -> FunctionGenerator [CmdSeq]
handle_int e nt = do
    (bin, other) <- gather_binary e 
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
    cret <- generate_exp val nt
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
    

sio = lift

generate_function :: Ast.FunctionSpec -> Ast.Function -> SeqGenerator ([CmdSeq], FunctionCall)
generate_function spec (Ast.Function fid params locals exp) = do
  flable <- s_alloc_label
  ce <- runStateT (generate_exp exp Return) $ FunctionGeneratorState locals
  ace <- assign_apply_labels $ fst ce
  return ((CmdSeq (Just $ NamedLabel flable $ Ast.fname spec) Skip:ace), 
    FunctionCall fid flable params (next_local $ snd ce) $ Ast.fname spec)


to_cmd_with_label :: Cmd -> Maybe (Int -> Cmd)
to_cmd_with_label (AddParamMove id) = return $ AddParamMoveWithLabel id
to_cmd_with_label (Cmd.Call id) = return $ CallWithLabel id
to_cmd_with_label _ = Nothing

assign_apply_labels :: [CmdSeq] -> SeqGenerator [CmdSeq] 
assign_apply_labels (h:t) = do
  at <- assign_apply_labels t
  case to_cmd_with_label $ cmd h of
    Nothing -> return (h:at)
    Just f -> do
      l <- s_alloc_label
      return $ [CmdSeq (label h) $ f l, CmdSeq (Just $ Label l) Skip] ++ at
assign_apply_labels [] = return [] 

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
  evalStateT (generate_program p) (SeqGeneratorState 0)



