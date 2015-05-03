module Abscthulhu where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)
data Program =
   RealProgram [TopDef]
  deriving (Eq,Ord,Show)

data TopDef =
   FnDef Ident Type Ident [Ident] Expr
 | FnDefTemplate Ident [DeclTemplateType] Type Ident [Ident] Expr
 | DataTypeDef Ident [Ident] [DataVariant]
  deriving (Eq,Ord,Show)

data DeclTemplateType =
   RealDeclTemplateType Ident
  deriving (Eq,Ord,Show)

data DataVariant =
   RealDataVariant Ident [DataVariantField]
  deriving (Eq,Ord,Show)

data DataVariantField =
   IdDataVariantField Ident
 | TypeDataVariantField Type
  deriving (Eq,Ord,Show)

data SimpleType =
   RealSimpleType Type
 | IdSimpleType Ident
  deriving (Eq,Ord,Show)

data Type =
   ComplexType Ident [SimpleType]
 | FnType Type Type
  deriving (Eq,Ord,Show)

data TypeListElem =
   RealTypeListElem Type
  deriving (Eq,Ord,Show)

data Expr =
   EVar Ident
 | EVarTemplate Ident [TypeListElem]
 | ELitInt Integer
 | EApply Expr Expr
 | ELet Ident Expr Expr
 | EIfElse Expr Expr Expr
 | ECase Expr [CaseVariant]
  deriving (Eq,Ord,Show)

data CaseVariant =
   RealCaseVariant Ident [Ident] Expr
  deriving (Eq,Ord,Show)
