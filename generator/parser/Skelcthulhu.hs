module Skelcthulhu where

-- Haskell module generated by the BNF converter

import Abscthulhu
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProgram :: Program -> Result
transProgram x = case x of
  RealProgram topdefs  -> failure x


transTopDef :: TopDef -> Result
transTopDef x = case x of
  FnDef id1 type'2 id3 ids4 expr5  -> failure x
  FnDefTemplate id1 decltemplatetypes2 type'3 id4 ids5 expr6  -> failure x
  DataTypeDef id ids datavariants  -> failure x


transDeclTemplateType :: DeclTemplateType -> Result
transDeclTemplateType x = case x of
  RealDeclTemplateType id  -> failure x


transDataVariant :: DataVariant -> Result
transDataVariant x = case x of
  RealDataVariant id datavariantfields  -> failure x


transDataVariantField :: DataVariantField -> Result
transDataVariantField x = case x of
  IdDataVariantField id  -> failure x
  TypeDataVariantField type'  -> failure x


transSimpleType :: SimpleType -> Result
transSimpleType x = case x of
  RealSimpleType type'  -> failure x
  IdSimpleType id  -> failure x


transType :: Type -> Result
transType x = case x of
  ComplexType id simpletypes  -> failure x
  FnType type'1 type'2  -> failure x


transTypeListElem :: TypeListElem -> Result
transTypeListElem x = case x of
  RealTypeListElem type'  -> failure x


transExpr :: Expr -> Result
transExpr x = case x of
  EVar id  -> failure x
  EVarTemplate id typelistelems  -> failure x
  ELitInt n  -> failure x
  EApply expr1 expr2  -> failure x
  ELet id expr1 expr2  -> failure x
  EIfElse expr1 expr2 expr3  -> failure x
  ECase expr casevariants  -> failure x


transCaseVariant :: CaseVariant -> Result
transCaseVariant x = case x of
  RealCaseVariant id ids expr  -> failure x



