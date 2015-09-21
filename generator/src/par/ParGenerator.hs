module ParGenerator where

import Ast
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import CmdGeneratorUtils
import Cmd
import Config
import Control.Monad.State
import Control.Monad.Trans.Reader

functions_map :: Program -> Map.Map Int Function
functions_map prog = 
  Map.fromList $ map (\x -> (Ast.fid $ snd x, snd x)) $ Map.toList $ functions prog 

called_functions :: Exp -> [Int]
called_functions (Ast.Construct _ e) = concat $ map called_functions e 
called_functions (Ast.Operator _ l r) = called_functions l ++ called_functions r
called_functions (Ast.Apply f p) = called_functions f ++ called_functions p
called_functions (Ast.Global _ _ e) = concat $ map called_functions e 
called_functions (Ast.Call fid e) = (fid : (concat $ map called_functions e))
called_functions (Ast.Let _ f p) = called_functions f ++ called_functions p
called_functions (Ast.If c t f) = called_functions c ++ called_functions t ++ called_functions f
called_functions (Ast.Case c o) = 
  called_functions c ++ (concat $ map (called_functions.case_exp) o)
called_functions _ = []

has_cycle :: Map.Map Int Function -> Int -> [Int] -> Bool
has_cycle fm f prev_called = case Map.lookup f fm of
  Nothing -> False
  Just fe -> let called = nub (called_functions $ Ast.exp fe) in
    case intersect called prev_called of
      (h:_) -> True
      [] -> or $ map (\k -> has_cycle fm k $ called ++ prev_called) called


complex_function :: Map.Map Int Function -> Int -> Bool
complex_function fm f = has_cycle fm f []

complex_functions :: Program -> Map.Map Int Bool
complex_functions p = let fm = functions_map p in 
  fmap (\x -> complex_function fm $ Ast.fid x)  fm

has_complex :: (Int -> Bool) -> Exp -> Bool
has_complex f (Ast.Construct _ e) = or $ map (has_complex f) e 
has_complex f (Ast.Operator _ l r) = has_complex f l || has_complex f r
has_complex f (Ast.Apply _ _) = True
has_complex f (Ast.Global _ _ e) = or $ map (has_complex f) e 
has_complex f (Ast.Call fid e) = f fid || (or $ map (has_complex f) e)
has_complex f (Ast.Let _ e p) = has_complex f e || has_complex f p
has_complex f (Ast.If c t e) = has_complex f c || has_complex f t || has_complex f e
has_complex f (Ast.Case c o) = has_complex f c || (or $ map ((has_complex f).case_exp) o)
has_complex _ _ = False


has_complex_call :: Exp -> FunctionGeneratorWithConfig Bool
has_complex_call e = do
  f <- lift $ lift $  gets CmdGeneratorUtils.is_complex
  return $ has_complex f e

is_complex_call :: Int -> FunctionGeneratorWithConfig Bool
is_complex_call id = do
  f <- lift $ lift $ gets CmdGeneratorUtils.is_complex
  return $ f id

-- some data that describes what happens after execution of current exp

-- some data that describes what should be done with this expression result
data Requirements = Requirements {
  need_type :: NeedType,
  complex_follows :: Bool,
  strict_expected :: Bool
}

handle_int :: Exp -> NeedType -> Bool -> FunctionGeneratorWithConfig ([CmdSeq], Bool)
handle_int e nt has_complex = do
    (bin, other, locs) <- gather_binary e 
    (cmds, stricts) <- prepare_parameters other ToEnv has_complex
    need <- arith_need nt
    let non_strict = map (\x -> Wait $ ttarget $ fst x) $ filter (not.snd) $ zip other stricts 
        wait_locals = map Wait $ nub locs in
      return (cmds ++ map no_label (non_strict ++ wait_locals ++ [Arith bin]) ++ need, True)

prepare_parameters :: [TargetedExp] -> (Int -> NeedType) -> Bool 
  -> FunctionGeneratorWithConfig ([CmdSeq], [Bool])
prepare_parameters [] _ _ = return ([], [])
prepare_parameters (h:t) nt complex_follows = do
  (cmds, stricts) <- prepare_parameters t nt complex_follows
  hc <- mapM (has_complex_call.texp) t
  (cms, strict) <- generate_exp (texp h) $ Requirements (nt $ ttarget h) (complex_follows || or hc) False
  return $ (cms ++ cmds, (strict:stricts))

handle_params :: [Exp] -> Cmd -> Cmd -> NeedType -> Bool -> FunctionGeneratorWithConfig [CmdSeq]
handle_params exps prep finalize nt complex_follows = do
    tparams <- mapM (\x -> alloc_local >>= \id -> return $ TargetedExp x id) exps
    cmds <- prepare_parameters tparams ToEnv complex_follows
    let pm = map (\e -> no_label $ PrepareParamMove (ttarget $ fst e) $ snd e) $ zip tparams [0..] in do
        fin <- acc_need nt
        return $ fst cmds ++ ([no_label prep] ++ pm ++ [no_label finalize]) ++ fin



generate_case :: CaseVariant -> Int -> Int -> Requirements -> FunctionGeneratorWithConfig ([CmdSeq], Bool)
generate_case (CaseVariant f e) ml el req = do
  let fields_move = map (\(fid, target) -> no_label $ StoreField fid target) $ zip [0..] f in do
    (ce, se) <- generate_exp e req
    return ([CmdSeq (Just ml) Skip Nothing] ++ fields_move ++ ce ++ [no_label $ Jmp el], se)


generate_exp :: Exp -> Requirements ->  FunctionGeneratorWithConfig ([CmdSeq], Bool)
generate_exp e req = case e of
  Ast.Construct id exps -> do
    c <- handle_params exps (AllocParams $ length exps) (Cmd.Construct id) (need_type req) (complex_follows req)
    return (c, True)
  Ast.Operator _ _ _ -> handle_int e (need_type req) (complex_follows req)
  Ast.Apply f p -> do
    id <- alloc_local
    nc <- has_complex_call f
    (pe, _) <- generate_exp p $ Requirements (ToEnv id) (nc || complex_follows req) False
    (fe, _) <- generate_exp f $ Requirements Acc True True
    fin <- acc_need $ need_type req
    if not (strict_expected req) && complex_follows req
      then
        return $ (pe ++ fe ++ [no_label $ AddParamMoveParFork id] ++ fin, False)
      else
        return $ (pe ++ fe ++ [no_label $ AddParamMove id] ++ fin, True)
  Ast.Const _ -> handle_int e (need_type req) $  complex_follows req
  Ast.Local id -> do
    fin <- acc_need $ need_type req
    if strict_expected req then
        return $ ([no_label $ Wait id, no_label $ Load id] ++ fin, True)
    else
        return $ ([no_label $ Load id] ++ fin, False)
  Ast.Global id p exps -> do
    cmds <- handle_params exps (AllocParams $ length exps) (Cmd.GlobalPar id) (need_type req) $  complex_follows req
    return (cmds, True)
  Ast.Call id exps -> do
    id_complex  <- is_complex_call id
    if not (strict_expected req) && (complex_follows req) && id_complex
      then do
        cmds <- handle_params exps (AllocFunctionEnv id) (Cmd.CallFork id) (need_type req) True
        return (cmds, False)
      else do
        is_tco <- asks tco
        finalize_command <- if is_tco then
            case need_type req of
              Return -> return Cmd.CallTail
              _ -> return Cmd.Call
          else
            return Cmd.Call
        cmds <- handle_params exps (AllocFunctionEnv id) (finalize_command id)  (need_type req)
          (complex_follows req || id_complex)
        return (cmds, True)
  Ast.Let id val ret -> do
    hc <- has_complex_call ret 
    (cval, _) <- generate_exp val $ Requirements (ToEnv id) (hc || complex_follows req) False
    (cret, s) <- generate_exp ret req
    return $ (cval ++ cret, s)
  Ast.If c t f -> do
    thc <- has_complex_call t
    fhc <- has_complex_call f
    (cc, _) <- generate_exp c $ Requirements ArithNeed (complex_follows req || thc || fhc) True
    (ct, st) <- generate_exp t req
    lf <- alloc_label
    (cf, sf) <- generate_exp f req
    le <- alloc_label
    return $ (cc ++ [no_label $ JmpIfZero lf] ++ ct ++ [no_label $ Jmp le]
       ++ [CmdSeq (Just lf) Skip Nothing] ++  cf ++ [CmdSeq (Just le) Skip Nothing], 
       st && sf)
  Ast.Case value cases -> do
    hc <- mapM (\x -> has_complex_call $ case_exp x) cases
    (cval, _) <- generate_exp value $ Requirements Acc (complex_follows req || or hc) True
    end_label <- alloc_label
    lcases <- mapM (\x -> alloc_label >>= \l -> return (x, l)) cases
    ccases <- mapM (\(mc, ml) -> generate_case mc ml end_label req)
      lcases 
    return (cval ++ [no_label $ JmpCase $ map snd lcases] ++ (concat $ map fst ccases) 
      ++ [CmdSeq (Just end_label) Skip Nothing], and $ map snd ccases )

generate_function :: Config -> Ast.FunctionSpec -> Ast.Function -> SeqGenerator ([CmdSeq], FunctionCall)
generate_function conf spec (Ast.Function fid params locals exp) = do
  flable <- s_alloc_label
  ce <- runStateT (runReaderT (generate_exp exp $ Requirements Return False True) conf) $ FunctionGeneratorState locals
  ace <- assign_apply_labels $ fst $ fst ce
  f <- gets CmdGeneratorUtils.is_complex
  return ((CmdSeq (Just flable) Skip (Just $ Ast.fname spec):ace) ++ [CmdSeq Nothing Skip $ Just $ "end of function " ++ Ast.fname spec], 
    FunctionCall fid flable params (next_local $ snd ce) (Ast.fname spec) $ f fid)

generate_program :: Config -> Program -> SeqGenerator Cmds
generate_program c prog = do
  cmds <- mapM (\(k, v) -> generate_function c k v) $ Map.toList $ Ast.functions prog
  let calls =  Map.fromList $ map (\(_, x) -> (Cmd.fid x, x)) cmds in do
    final_label <- s_alloc_label
    case Map.lookup 0 calls of
      Just x -> return $ Cmds ((concat $ map fst cmds) ++ [CmdSeq (Just final_label) 
        Finalize Nothing]) calls x final_label
      Nothing -> sio $ ioError $ userError "Unknown function - should not happen" 

generate :: Config -> Program -> IO Cmds
generate c p = let f = \k -> Map.findWithDefault False k $ complex_functions p in 
  evalStateT (generate_program c p) (SeqGeneratorState 0 f)


