module Cmd where

import qualified Data.Map as Map

data OpType = Add | Sub | Lt | Le | Gt | Ge | Eq | Neq | And | Or | Mul
    deriving (Eq, Ord, Show)

data ArithOp = 
  Operation OpType ArithOp ArithOp
  | Local Int
  | Const Integer
    deriving (Eq, Ord, Show)

data Cmd =
    Load Int
    | AllocParams Int
    | AllocFunctionEnv Int
    | PrepareParamMove Int Int
    | JmpIfZero Int
    | Jmp Int
    | AddParamMove Int
    | AddParamMoveParFork Int
    | AddParamMoveWithLabel Int Int
    | AddParamMoveParForkWithLabel Int Int
    | Wait Int
    | WaitWithLabel Int Int
    | Arith ArithOp
    | StoreArith Int
    | LoadArith
    | ArithLoadAcc
    | Construct Int
    | Store Int
    | Call Int
    | CallTail Int
    | CallFork Int
    | CallWithLabel Int Int
    | CallForkWithLabel Int Int
    | Global Int
    | GlobalPar Int
    | StoreField Int Int
    | JmpCase [Int]
    | Ret
    | Skip
    | Finalize
    deriving (Eq, Ord, Show)



data FunctionCall = FunctionCall {
  fid :: Int,
  flabel :: Int,
  params :: Int,
  env_size :: Int,
  fname :: String,
  is_complex :: Bool
} deriving (Eq,Ord,Show)

data CmdSeq = CmdSeq {
  label :: Maybe Int,
  cmd :: Cmd,
  comment :: Maybe String
} deriving (Eq, Ord, Show)

data Cmds = Cmds [CmdSeq] (Map.Map Int FunctionCall) FunctionCall Int
    deriving (Eq, Ord, Show)

