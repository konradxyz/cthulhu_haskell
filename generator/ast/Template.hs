module Template where

import qualified Abscthulhu as Abs
import qualified Data.Map as Map

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



data VariantTemplate = VariantTemplate { vname :: String, vparam_count :: Int }
  deriving (Show, Eq, Ord)


data Templates = Templates { functions :: Map.Map String FunctionTemplate,
                             variant   :: Map.Map String VariantTemplate }
  deriving (Show, Eq, Ord)
