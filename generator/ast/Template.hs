module Template where

import qualified Abscthulhu as Abs
import qualified Data.Map as Map
import qualified Ast as Ast

data Type = Int | FnType Type Type | VariantType String [Type] | Param Int
  deriving (Show, Eq, Ord)


data FunctionTemplate = FunctionTemplate 
    { name        :: String
    , param_count :: Int --Template params
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
  vconstructors :: [Abs.DataVariant]
  vconstructorsc :: [Constructor]
} deriving (Show, Eq, Ord)



data Templates = Templates { functions :: Map.Map String FunctionTemplate,
                             variant   :: Map.Map String VariantTemplateConst,
                             globals   :: Map.Map String Global }
  deriving (Show, Eq, Ord)
