module Ast where

data Type = Int | Function Type Type | Variant String [Type]

data Exp = 
  Operator OpType Exp Exp | 
  Apply Exp Exp |
  Local Int |
  Global Int |
  Let Int Exp Exp |
  If Exp Exp Exp |
  Case Exp [CaseVariant]
data Expr =
   EVar Ident
 | EVarTemplate Ident [TypeListElem]
 | ELitInt Integer
 | EApply Expr Expr
 | ELet Ident Expr Expr
 | EIfElse Expr Expr Expr
 | ECase Expr [CaseVariant]
  deriving (Eq,Ord,Show)


data Program = PProgram
