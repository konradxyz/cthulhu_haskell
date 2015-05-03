module Template where

import qualified Abscthulhu as Abs
import qualified Data.Map as Map
import qualified Ast as Ast
import Data.List

data Type = Int | FnType Type Type | VariantType String [Type] | Param Int
  deriving (Show, Eq, Ord)


data FunctionTemplate = FunctionTemplate 
    { name        :: String
    , param_count :: Int --Template params
    , template_params :: [String]
    , ftype       :: Type
    , params      :: [String]
    , exp         :: Abs.Expr
    }
  deriving (Show, Eq, Ord)

data FunctionInstance = FunctionInstance { fname :: String, type_params :: [Ast.Type] }
  deriving (Show, Eq, Ord)


data Global = FunctionGlobal | ConstructorGlobal VariantTemplateConst | BinOp Ast.OpType
  deriving (Show, Eq, Ord)


data VariantTemplate = VariantTemplate { 
  vname :: String,
  vparams :: [String],
  vconstructors :: [Abs.DataVariant]
}  deriving (Show, Eq, Ord)

data Constructor = Constructor {
  cname :: String,
  cfields :: [Type]
} deriving (Show, Eq, Ord)

data VariantTemplateConst = VariantTemplateConst {
  vnamec :: String,
  vparamsc :: [String],
  vconstructorsc :: [Constructor]
} deriving (Show, Eq, Ord)


get_constructor :: String -> VariantTemplateConst -> Maybe (Constructor, Int)
get_constructor name vtc =
  find (\(c,_) -> cname c == name) $ zip (vconstructorsc vtc) [0..]  


data Templates = Templates { functions :: Map.Map String FunctionTemplate,
                             variant   :: Map.Map String VariantTemplateConst,
                             variant_spec :: Map.Map String VariantTemplate,
                             globals   :: Map.Map String Global }
  deriving (Show, Eq, Ord)
