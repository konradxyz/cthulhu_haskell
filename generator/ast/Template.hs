module Template where


data Type = Int | FnType Type Type | Variant String [Type] | Param Type

data FunctionTemplate = FunctionTemplate 
    { name        :: String
    , param_count :: Int
    , ftype       :: Type
    }




data VariantTemplate = VariantTemplate { vname :: String, vparam_count :: Int }
  deriving (Show, Eq, Ord)
