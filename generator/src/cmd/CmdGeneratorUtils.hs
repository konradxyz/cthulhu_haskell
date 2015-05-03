module CmdGeneratorUtils where

import Ast
import Cmd
import Control.Monad.State

data SeqGeneratorState = SeqGeneratorState {
  next_label :: Int,
  is_complex :: Int -> Bool
}

instance Show SeqGeneratorState where
  show f = "next_label: " ++ (show $ next_label f)

type SeqGenerator = StateT SeqGeneratorState IO


data FunctionGeneratorState = FunctionGeneratorState {
  next_local :: Int
} deriving (Show)

type FunctionGenerator = StateT FunctionGeneratorState SeqGenerator


data TargetedExp = TargetedExp {
  texp :: Exp,
  ttarget :: Int
}

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


no_label :: Cmd -> CmdSeq
no_label c = CmdSeq Nothing c
m_nl = map no_label



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





gather_binary :: Exp -> FunctionGenerator (ArithOp, [TargetedExp], [Int])
gather_binary e = case e of
  Operator op l r -> do
    (la, lt, ll) <- gather_binary l
    (ra, rt, rl) <- gather_binary r
    return (Operation (op_ast_to_cmd op) la ra, lt ++ rt, ll ++ rl)
  Ast.Const n -> return $ (Cmd.Const n, [], [])
  Ast.Local id -> return $ (Cmd.Local id, [], [id])
  _ -> do
    id <- alloc_local
    return $ (Cmd.Local id, [TargetedExp e id], [])
 

to_cmd_with_label :: Cmd -> Maybe (Int -> Cmd)
to_cmd_with_label (AddParamMove id) = return $ AddParamMoveWithLabel id
to_cmd_with_label (Cmd.Call id) = return $ CallWithLabel id
to_cmd_with_label (Cmd.AddParamMoveParFork id) = return $ AddParamMoveParForkWithLabel id
to_cmd_with_label (Cmd.CallFork id) = return $ CallForkWithLabel id
to_cmd_with_label (Cmd.Wait id) = return $ WaitWithLabel id
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

sio :: IO a -> SeqGenerator a
sio = lift

