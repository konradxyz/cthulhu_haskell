module Ast where
import qualified Data.Map as Map

data Type = IntType | FunctionType Type Type | VariantType Int
  deriving (Eq, Show, Ord)

data CaseVariant = CaseVariant {
  constructor :: Int,
  case_exp    :: Exp
} deriving (Eq, Show, Ord)

data OpType = Add | Sub | Lt
  deriving (Eq, Show, Ord)


data Exp = 
  Constructor Int [Exp] |
  Operator OpType Exp Exp | 
  Apply Exp Exp |
  Const Int |
  Param Int |
  Local Int |
  Global Int |
  Let Int Exp Exp |
  If Exp Exp Exp |
  Case Exp [CaseVariant]
  deriving (Eq, Show, Ord)

data Function = Function {
  fid  :: Int,
  params_count :: Int,
  exp :: Exp
} deriving (Eq, Show, Ord)

data Constructor = Constructor {
  cid :: Int,
  fields :: [Type]
} deriving (Eq, Show, Ord)

data Variant = Variant {
  vid :: Int,
  constructors :: [Constructor]
} deriving (Eq, Show, Ord)

data FunctionSpec = FunctionSpec {
  fname :: String,
  templateParams :: [Type]
} deriving (Eq, Show, Ord)

data VariantSpec = VariantSpec {
  vname :: String,
  vtemplateParams :: [Type]
} deriving (Eq, Show, Ord)

data Program = Program { functions :: Map.Map FunctionSpec Function, types :: [Variant] }
  deriving (Eq, Show, Ord)
