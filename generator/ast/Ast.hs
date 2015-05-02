module Ast where
import qualified Data.Map as Map
import Data.List
data Type = IntType | FunctionType Type Type | VariantType Int
  deriving (Eq, Show, Ord)

data CaseVariant = CaseVariant {
  fields_target :: [Int],
  case_exp    :: Exp
} deriving (Eq, Show, Ord)

data OpType = Add | Sub | Lt | Le | Gt | Ge | Eq | Neq | And | Or | Mul
  deriving (Eq, Show, Ord)


data Exp = 
  Construct Int [Exp] |
  Operator OpType Exp Exp | 
  Apply Exp Exp |
  Const Integer |
  Local Int |
  Global Int Int [Exp] |
  Call Int [Exp] |
  Let Int Exp Exp |
  If Exp Exp Exp |
  Case Exp [CaseVariant]
  deriving (Eq, Show, Ord)

data Function = Function {
  fid  :: Int,
  params_count :: Int,
  locals_count :: Int,
  exp :: Exp
} deriving (Eq, Show, Ord)

data Constructor = Constructor {
  cid :: Int,
  cname :: String,
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

data Program = Program {functions :: Map.Map FunctionSpec Function, types :: Map.Map Int Variant}
  deriving (Eq, Ord)

instance Show Program where
    show (Program f t) = intercalate "\n" $ ["functions:"] ++ (map show $ Map.toList f) ++ 
                         ["types"] ++  (map show $ Map.toList t)
