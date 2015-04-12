module Typechecker where

import Ast
import qualified Abscthulhu as Abs
import Template
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Error
import Utils

to_string :: Abs.Ident -> String
to_string (Abs.Ident str) = str

predefined_types :: Map.Map String Template.Type
predefined_types = Map.insert "Int" Template.Int Map.empty

type Ident = Abs.Ident

type TemplateTypeableEnv a = 
  ErrorT String (Reader (String -> [Template.Type] -> Either String Template.Type)) a
  
class TemplateTypeable a where
  template_type :: a -> TemplateTypeableEnv Template.Type

instance TemplateTypeable Abs.SimpleType where
  template_type (Abs.IdSimpleType id) = do
    fun <- ask
    case fun (to_string id) [] of
      Right r -> return r
      Left e -> throwError e
  template_type (Abs.RealSimpleType tp) = template_type tp

instance TemplateTypeable Abs.Type where
  template_type (Abs.FnType param res) = do
    p <- template_type param
    r <- template_type res
    return $ FnType p r
  template_type (Abs.ComplexType id params) = do
    pars <- mapM template_type params
    fun <- ask
    case fun (to_string id) pars of
      Right r -> return r
      Left e -> throwError e


function_template_type :: Ident -> Ident -> Abs.Type -> [Ident] 
    -> TemplateTypeableEnv Template.FunctionTemplate
function_template_type name1 name2 tp params =
  if name1 == name2 then do
    ttp <- template_type tp
    return $ Template.FunctionTemplate (to_string name1) (length params) ttp 
  else
    throwError $ "Inconsistent functions name: " ++ to_string name1 ++ " and " ++ to_string name2

topdef_template_type :: Abs.TopDef -> TemplateTypeableEnv (Maybe Template.FunctionTemplate)
topdef_template_type (Abs.DataTypeDef _ _ _) = return Nothing
topdef_template_type (Abs.FnDef name1 tp name2 _ _) = 
  fmap Just $ function_template_type name1 name2 tp [] 
topdef_template_type (Abs.FnDefTemplate name1 template_params tp name2 _ _) =
  fmap Just $ function_template_type name1 name2 tp $
    map (\x -> let Abs.RealDeclTemplateType id = x in id) template_params


topdef_variant_type :: Abs.TopDef -> Either String (Maybe Template.VariantTemplate)
topdef_variant_type (Abs.DataTypeDef name param _) = 
  case Map.lookup (to_string name) predefined_types of
    Just _ -> throwError $ "Trying to redefine builtin type " ++ to_string name
    Nothing -> return $ Just $ Template.VariantTemplate (to_string name) (length param)
topdef_variant_type _ = return Nothing



variants :: Abs.Program -> Either String (Map.Map String Template.VariantTemplate)
variants (Abs.RealProgram program) = do
  vars <- fmap catMaybes $ mapM topdef_variant_type program
  case Utils.unique vname vars of
    Left (k, _, _)  -> throwError $ "Multiple definitions of " ++ k
    Right mp -> Right mp

-- | DataTypeDef Ident [Ident] [DataVariant]
-- Map String Type
-- Map String Int 
-- Map String Type
-- Map String 




    
{-
template_type :: Abscthulhu.Type -> Template.Type
template_type  (Abscthulhu.ComplexType id 
 | FnType Type Type
 -}
--template_type _ = Template.Int

typecheck :: Abs.Program -> Ast.Program
typecheck _ = PProgram 
