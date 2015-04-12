module Ast where

data Type = Int | Function Type Type | Variant String [Type]

--data Exp = Operator OpType Exp Exp | 

data Program = PProgram
