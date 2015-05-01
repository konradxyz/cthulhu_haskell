module Cmd where

import qualified Data.Map as Map

data OpType = Add | Sub | Lt
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
    | AddParamMoveWithLabel Int Int
    | Arith ArithOp
    | StoreArith Int
    | LoadArith
    | ArithLoadAcc
    | Construct Int
    | Store Int
    | Call Int
    | CallWithLabel Int Int
    | Global Int
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
  fname :: String
} deriving (Eq,Ord,Show)

data Label = Label Int | NamedLabel Int String
  deriving (Eq,Ord,Show)

data CmdSeq = CmdSeq {
  label :: Maybe Label,
  cmd :: Cmd
} deriving (Eq, Ord, Show)

data Cmds = Cmds [CmdSeq] (Map.Map Int FunctionCall) FunctionCall Int
    deriving (Eq, Ord, Show)

