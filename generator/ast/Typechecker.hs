module Typechecker where
import Data.List
import Ast
import qualified Abscthulhu as Abs
import Template
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Error
import Utils
import Control.Monad.Trans.Reader
to_string :: Abs.Ident -> String
to_string (Abs.Ident str) = str

predefined_types :: Map.Map String Template.Type
predefined_types = Map.insert "int" Template.Int Map.empty

operators :: [String]
operators = ["add", "sub", "lt"]

type Ident = Abs.Ident

--type TemplateTypeableEnv a = 
--  ErrorT String (Reader (String -> [Template.Type] -> Either String Template.Type)) a

type TemplateTypeableEnv a = 
  ReaderT (String -> [Template.Type] -> Either String Template.Type) (Either String) a


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


add_template_params :: 
  [Ident] -> (String -> [Template.Type] -> Either String Template.Type) ->
  String -> [Template.Type] -> Either String Template.Type
add_template_params params fallback name subtypes =
  case elemIndex name $ map to_string params of
    Just id -> case subtypes of
      (h:_) -> throwError $ "Template parameter should not be given parameters " ++ name
      [] -> return $ Template.Param id
    Nothing -> fallback name subtypes

function_template_type :: Ident -> Ident -> Abs.Type -> [Ident] -> [Ident] -> Abs.Expr 
    -> TemplateTypeableEnv Template.FunctionTemplate
function_template_type name1 name2 tp params args e =
  if name1 == name2 then do
    ttp <- local (add_template_params params) $ template_type tp
    return $ Template.FunctionTemplate (to_string name1) (length params) ttp (map to_string args) e
  else
    throwError $ "Inconsistent functions name: " ++ to_string name1 ++ " and " ++ to_string name2

topdef_template_type :: Abs.TopDef -> TemplateTypeableEnv (Maybe Template.FunctionTemplate)
topdef_template_type (Abs.DataTypeDef _ _ _) = return Nothing
topdef_template_type (Abs.FnDef name1 tp name2 args e) = 
  fmap Just $ function_template_type name1 name2 tp [] args e
topdef_template_type (Abs.FnDefTemplate name1 template_params tp name2 args e) =
  fmap Just $ function_template_type name1 name2 tp 
    (map (\x -> let Abs.RealDeclTemplateType id = x in id) template_params) args e


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

get_type :: Map.Map String Template.VariantTemplate -> 
            Map.Map String Template.Type ->  
            String -> [Template.Type] -> Either String Template.Type
get_type variants predefined name subtypes = case Map.lookup name predefined of
  Just x -> case subtypes of
    [] -> return x
    (_:_) -> throwError $ "Predefined type does not allow parameters " ++ name
  Nothing -> case Map.lookup name variants of
    Just x -> if length subtypes == vparam_count x then
        return $ Template.VariantType (vname x) subtypes
      else
        throwError $ "Wrong number of parameters given to variant type " ++ name
    Nothing -> throwError $ "Unknown type " ++ name 

functions :: 
  (String -> [Template.Type] -> Either String Template.Type) -> 
  Abs.Program -> Either String (Map.Map String Template.FunctionTemplate)
functions fun (Abs.RealProgram program) = do 
  funs <- fmap catMaybes $ mapM (\x -> runReaderT (topdef_template_type x) fun)  program
  case Utils.unique name funs of
    Left (f, _, _) -> throwError $ "Multiple definitions of function " ++ f
    Right m -> case Utils.exists operators m of
      Just id -> throwError $ "Trying to redefine builtin operator " ++ id
      Nothing -> return m

templates :: Abs.Program -> Either String Template.Templates
templates program = do
  vars <- variants program
  funs <- Typechecker.functions (get_type vars predefined_types) program
  return $ Template.Templates funs vars



    
{-
template_type :: Abscthulhu.Type -> Template.Type
template_type  (Abscthulhu.ComplexType id 
 | FnType Type Type
 -}
--template_type _ = Template.Int

typecheck :: Abs.Program -> Ast.Program
typecheck _ = PProgram 
