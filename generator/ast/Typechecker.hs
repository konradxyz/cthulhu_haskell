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
import Control.Monad.Trans.State.Lazy

to_string :: Abs.Ident -> String
to_string (Abs.Ident str) = str

predefined_types :: Map.Map String Template.Type
predefined_types = Map.insert "int" Template.Int Map.empty

operators :: [String]
operators = ["add", "sub", "lt"]

operators_ast = [Ast.Add, Ast.Sub, Ast.Lt]

operators_map = Map.fromList $ zip operators operators_ast

type Ident = Abs.Ident

type TemplateTypeableEnv a = 
  ReaderT (String -> [Template.Type] -> Either String Template.Type) (Either String) a


class TemplateTypeable a where
  template_type :: a -> TemplateTypeableEnv Template.Type
  get_template_type ::  (String -> [Template.Type] -> Either String Template.Type) 
    -> a -> Either String Template.Type
  get_template_type f x = runReaderT (template_type x) f

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

instance TemplateTypeable Abs.DataVariantField where
  template_type (Abs.IdDataVariantField id) = template_type (Abs.IdSimpleType id)
  template_type (Abs.TypeDataVariantField tp) = template_type tp


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



topdef_variant_type :: Abs.TopDef 
    -> Either String (Maybe Template.VariantTemplate)
topdef_variant_type (Abs.DataTypeDef name param constructors) = 
  case Map.lookup (to_string name) predefined_types of
    Just _ -> throwError $ "Trying to redefine builtin type " ++ to_string name
    Nothing -> do
      return $ Just $ 
        (Template.VariantTemplate (to_string name) (map to_string param) constructors)
topdef_variant_type _ = return Nothing



variants :: Abs.Program -> Either String (Map.Map String Template.VariantTemplate)
variants (Abs.RealProgram program) = do
  vars <- fmap catMaybes $ mapM topdef_variant_type program
  case Utils.unique Template.vname vars of
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
    Just x -> if length subtypes == (length.vparams) x then
        return $ Template.VariantType (Template.vname x) subtypes
      else
        throwError $ "Wrong number of parameters given to variant type " ++ name
    Nothing -> throwError ("Unknown type " ++ name)

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

variant_constructor :: (String -> [Template.Type] -> Either String Template.Type) ->
  Abs.DataVariant -> Either String Template.Constructor
variant_constructor f (Abs.RealDataVariant (Abs.Ident name) fields)  = do
  cfields <- mapM (get_template_type f) fields
  return $ Template.Constructor name cfields

variant_template :: (String -> [Template.Type] -> Either String Template.Type) ->
  Template.VariantTemplate -> Either String Template.VariantTemplateConst
variant_template f (Template.VariantTemplate name params const) = do
  vconsts <- mapM (variant_constructor (add_template_params (map Abs.Ident params) f)) const
  return $ Template.VariantTemplateConst name params vconsts 



global_constructors :: Template.VariantTemplateConst -> [(String, Global)]
global_constructors v =
  zip (map cname $ vconstructorsc v) (replicate (length $ vconstructorsc v) $ ConstructorGlobal v)  

{-


globals_const :: 
  (String -> [Template.Type] -> Either String Template.Type) -> 

Map.Map String Template.Global -> 

globals :: Map.Map String Template.VariantTemplat

templates
-}

templates :: Abs.Program -> Either String Template.Templates
templates program = do
  vars <- variants program
  let (keys, values) = unzip $ Map.toList vars in do
    variants <- mapM (variant_template (get_type vars predefined_types)) values
    funs <- Typechecker.functions (get_type vars predefined_types) program
    let gfuns = (map (\x->(Template.name $ snd x, FunctionGlobal)) $ Map.toList funs)
        gcons = concat $ map (global_constructors) $ variants
        gops =  zip operators $ map Template.BinOp operators_ast in  
        case Utils.unique_v fst snd $ gfuns ++ gcons ++ gops of
          Left (e, _, _) -> throwError $ "Multiple definitions of ident " ++ e
          Right global -> do
            return $ Template.Templates funs (Map.fromList $ zip keys variants)  global



data TypecheckerState = TypecheckerState {
  functions_done :: Map.Map FunctionSpec Int,
  functions_todo :: Map.Map FunctionSpec Int,
  variants_done :: Map.Map VariantSpec Int,
  program :: Program
}


starting_state :: TypecheckerState
starting_state = TypecheckerState {
  functions_done = Map.empty,
  functions_todo = Map.insert (FunctionSpec "call" []) 0 Map.empty,
  variants_done = Map.empty,
  program = Program Map.empty []
} 

type Typechecker = StateT TypecheckerState (ReaderT Template.Templates (Either String))

get_add_function_id :: FunctionSpec -> Typechecker Int
get_add_function_id f = do
  done <- gets functions_done
  case Map.lookup f done of
    Just r -> return r
    Nothing -> do
      todo <- gets functions_todo
      case Map.lookup f todo of
        Just rt -> return rt
        Nothing -> do 
          let new_id = (Map.size done) + (Map.size todo) in do
            state <- get
            put $ state { functions_todo = Map.insert f new_id todo }
            return new_id
            

data TypedExp = TypedExp {
  etype :: Ast.Type,
  eexp :: Ast.Exp
}


data FunctionTypecheckerEnv = FunctionTypecheckerEnv {
  locals :: Map.Map String TypedExp,
  next_local :: Int,
  template_params :: [Ast.Type]
}

type FunctionTypechecker = StateT FunctionTypecheckerEnv Typechecker

{-
data Expr =
   EVar Ident
 | EVarTemplate Ident [TypeListElem]
 | ELitInt Integer
 | EApply Expr Expr
 | ELet Ident Expr Expr
 | EIfElse Expr Expr Expr
 | ECase Expr [CaseVariant]
  deriving (Eq,Ord,Show)
-}


typed_apply :: TypedExp -> TypedExp -> FunctionTypechecker TypedExp
typed_apply function param =
  case etype function of
    FunctionType pt rt -> 
      if pt == etype param then
        return $ TypedExp { etype = rt, eexp = Apply (eexp function) (eexp param) } 
      else
        throwError "Params inconsistent"
    _ -> throwError "Too many parameters"

assert :: Bool -> String -> FunctionTypechecker ()
assert c msg = if c then return () else throwError msg

typecheck_exp_global :: String -> [Ast.Type] -> [TypedExp] -> FunctionTypechecker TypedExp
typecheck_exp_global name template_params params = do
  globals <- asks Template.globals
  case Map.lookup name globals of
    Nothing -> throwError $ "Unknown identifier " ++ name
    Just x -> case x of
      BinOp op ->  case params of
        [TypedExp Ast.IntType l, TypedExp Ast.IntType r] -> do
          assert (null template_params) "Binary operator should not be given template params"
          return $ TypedExp Ast.IntType (Operator op l r)
         _ -> throwError "Operator should be given two integer arguments"
      ConstructorGlobal tp -> do
        assert (length (vparamsc tp) == length template_params ) "Wrong number of template arguments" 
        
        state <- get
        case 
   
  case Map.lookup name operators_map of
    Just op -> case params of
      [TypedExp Ast.IntType l, TypedExp Ast.IntType r] -> do
        assert (null template_params) "Binary operator should not be given template params"
        return $ TypedExp Ast.IntType (Operator op l r)
      _ -> throwError "Operator should be given two arguments"
    Nothing -> do 
      local <- gets locals
      case Map.lookup name local of
        Just x -> do 
          assert (null template_params) "Local should not be given template params"
          foldM typed_apply x params
        Nothing -> do
--          id <- get_add_function_id $ FunctionSpec name [] 
          throwError "Not yet implemented"
 
typecheck_exp :: Abs.Expr -> [TypedExp]
  -> FunctionTypechecker TypedExp
typecheck_exp (Abs.EVar (Abs.Ident name)) params =
  case Map.lookup name operators_map of
    Just op -> case params of
      [TypedExp Ast.IntType l, TypedExp Ast.IntType r] -> do
        return $ TypedExp Ast.IntType  (Operator op l r)
      _ -> throwError "Operator should be given two arguments"
    Nothing -> do
    local <- gets locals
    case Map.lookup name local of
      Just x -> return x
      Nothing -> throwError "Not yet implemented"
      
typecheck_exp _  _ = return $ TypedExp Ast.IntType (Ast.Param 2) 


type_instance :: Template.Type -> [Ast.Type] -> Typechecker Ast.Type
type_instance Template.Int _ = return $ Ast.IntType
type_instance (Template.FnType p r) l = do
  tp <- type_instance p l
  tr <- type_instance r l
  return $ Ast.FunctionType tp tr
type_instance (Template.Param id) env  = if id >= length env then
    throwError $ "Too little template params"
  else
    return $ env!!id
type_instance (Template.VariantType name subtypes) env = do
  subs <- mapM (\x -> type_instance x env) subtypes
  types <- gets variants_done
  case Map.lookup (VariantSpec name subs) types of
    Just done -> return $ Ast.VariantType done
    Nothing -> do
      state <- get
      put $ state { variants_done = Map.insert (VariantSpec name subs) (Map.size types) types }
      return $ Ast.VariantType (Map.size types)



generate_params :: [String] -> Int -> Ast.Type -> Typechecker (Map.Map String TypedExp , Ast.Type)
generate_params [] _ tr = return (Map.empty, tr)
generate_params (h:t) id tr = case tr of
  Ast.FunctionType tp tr -> do
    (l, r) <- generate_params t (id + 1)  tr
    return (Map.insert h (TypedExp { etype = tp, eexp = Ast.Param id }) l, r)
  _ -> throwError "Function is defined to take too many arguments"

typecheck_function :: FunctionSpec -> Int -> Typechecker Function
typecheck_function f id = do
  env <- lift ask
  case Map.lookup (Ast.fname f) $ Template.functions env of
    Nothing -> throwError $ "Unknown function " ++ (Ast.fname f)
    Just template -> do
      ftype <- type_instance (Template.ftype template) (Ast.templateParams f) 
      (t_params, ret) <- generate_params (Template.params template) 0 ftype
      let f_state = FunctionTypecheckerEnv {
        locals = t_params,
        next_local = Map.size t_params,
        template_params = Ast.templateParams f } in 
         return $ Ast.Function id 2 (Ast.Param 1)


run_typecheck :: Typechecker Program
run_typecheck = do
  todo <- gets functions_todo
  case Map.assocs todo of
    ((spec, id) : _) -> do
      done <- gets functions_done
      state <- get
      put $ state { 
        functions_todo = Map.delete spec todo, 
        functions_done = Map.insert spec id done }
      func <- typecheck_function spec id
      state <- get
      let prog = program state in do 
        put $ state { 
          program = prog { Ast.functions = Map.insert spec func $ Ast.functions prog } }
        run_typecheck
    [] -> gets program
      
    
{-
template_type :: Abscthulhu.Type -> Template.Type
template_type  (Abscthulhu.ComplexType id 
 | FnType Type Type
 -}
--template_type _ = Template.Int

typecheck :: Abs.Program -> Either String Ast.Program
typecheck input = do
  temps <- templates input
  r <- runReaderT (runStateT run_typecheck starting_state) temps
  return $ fst r
  
  
  --TODO: make sure call has good type
