-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parcthulhu where
import Abscthulhu
import Lexcthulhu
import ErrM

}

%name pProgram Program
%name pTopDef TopDef
%name pListTopDef ListTopDef
%name pListIdent ListIdent
%name pDeclTemplateType DeclTemplateType
%name pListDeclTemplateType ListDeclTemplateType
%name pDataVariant DataVariant
%name pListDataVariant ListDataVariant
%name pDataVariantField DataVariantField
%name pListDataVariantField ListDataVariantField
%name pSimpleType SimpleType
%name pType1 Type1
%name pType Type
%name pListSimpleType ListSimpleType
%name pTypeListElem TypeListElem
%name pListTypeListElem ListTypeListElem
%name pExpr2 Expr2
%name pExpr1 Expr1
%name pExpr Expr
%name pCaseVariant CaseVariant
%name pListCaseVariant ListCaseVariant

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '(' { PT _ (TS _ 1) }
 ')' { PT _ (TS _ 2) }
 ',' { PT _ (TS _ 3) }
 '->' { PT _ (TS _ 4) }
 ':' { PT _ (TS _ 5) }
 '::' { PT _ (TS _ 6) }
 ';' { PT _ (TS _ 7) }
 '<' { PT _ (TS _ 8) }
 '=' { PT _ (TS _ 9) }
 '>' { PT _ (TS _ 10) }
 'case' { PT _ (TS _ 11) }
 'data' { PT _ (TS _ 12) }
 'else' { PT _ (TS _ 13) }
 'if' { PT _ (TS _ 14) }
 'in' { PT _ (TS _ 15) }
 'let' { PT _ (TS _ 16) }
 'of' { PT _ (TS _ 17) }
 'then' { PT _ (TS _ 18) }
 '|' { PT _ (TS _ 19) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }

Program :: { Program }
Program : ListTopDef { RealProgram (reverse $1) } 


TopDef :: { TopDef }
TopDef : Ident '::' Type ':' Ident ListIdent '=' Expr ';' { FnDef $1 $3 $5 (reverse $6) $8 } 
  | Ident '<' ListDeclTemplateType '>' '::' Type ':' Ident ListIdent '=' Expr ';' { FnDefTemplate $1 $3 $6 $8 (reverse $9) $11 }
  | 'data' Ident ListIdent '=' ListDataVariant ';' { DataTypeDef $2 (reverse $3) $5 }


ListTopDef :: { [TopDef] }
ListTopDef : {- empty -} { [] } 
  | ListTopDef TopDef { flip (:) $1 $2 }


ListIdent :: { [Ident] }
ListIdent : {- empty -} { [] } 
  | ListIdent Ident { flip (:) $1 $2 }


DeclTemplateType :: { DeclTemplateType }
DeclTemplateType : Ident { RealDeclTemplateType $1 } 


ListDeclTemplateType :: { [DeclTemplateType] }
ListDeclTemplateType : {- empty -} { [] } 
  | DeclTemplateType { (:[]) $1 }
  | DeclTemplateType ',' ListDeclTemplateType { (:) $1 $3 }


DataVariant :: { DataVariant }
DataVariant : Ident ListDataVariantField { RealDataVariant $1 (reverse $2) } 


ListDataVariant :: { [DataVariant] }
ListDataVariant : DataVariant { (:[]) $1 } 
  | DataVariant '|' ListDataVariant { (:) $1 $3 }


DataVariantField :: { DataVariantField }
DataVariantField : Ident { IdDataVariantField $1 } 
  | '(' Type ')' { TypeDataVariantField $2 }


ListDataVariantField :: { [DataVariantField] }
ListDataVariantField : {- empty -} { [] } 
  | ListDataVariantField DataVariantField { flip (:) $1 $2 }


SimpleType :: { SimpleType }
SimpleType : '(' Type ')' { RealSimpleType $2 } 
  | Ident { IdSimpleType $1 }


Type1 :: { Type }
Type1 : Ident ListSimpleType { ComplexType $1 (reverse $2) } 
  | '(' Type ')' { $2 }


Type :: { Type }
Type : Type1 '->' Type { FnType $1 $3 } 
  | Type1 { $1 }


ListSimpleType :: { [SimpleType] }
ListSimpleType : {- empty -} { [] } 
  | ListSimpleType SimpleType { flip (:) $1 $2 }


TypeListElem :: { TypeListElem }
TypeListElem : Type { RealTypeListElem $1 } 


ListTypeListElem :: { [TypeListElem] }
ListTypeListElem : TypeListElem { (:[]) $1 } 
  | TypeListElem ',' ListTypeListElem { (:) $1 $3 }


Expr2 :: { Expr }
Expr2 : Ident { EVar $1 } 
  | Ident '<' ListTypeListElem '>' { EVarTemplate $1 $3 }
  | Integer { ELitInt $1 }
  | '(' Expr ')' { $2 }


Expr1 :: { Expr }
Expr1 : Expr1 Expr2 { EApply $1 $2 } 
  | Expr2 { $1 }


Expr :: { Expr }
Expr : 'let' Ident '=' Expr1 'in' Expr { ELet $2 $4 $6 } 
  | 'if' Expr1 'then' Expr 'else' Expr { EIfElse $2 $4 $6 }
  | 'case' Expr1 'of' ListCaseVariant { ECase $2 (reverse $4) }
  | Expr1 { $1 }


CaseVariant :: { CaseVariant }
CaseVariant : Ident ListIdent '->' Expr { RealCaseVariant $1 (reverse $2) $4 } 


ListCaseVariant :: { [CaseVariant] }
ListCaseVariant : {- empty -} { [] } 
  | ListCaseVariant CaseVariant ';' { flip (:) $1 $2 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}
