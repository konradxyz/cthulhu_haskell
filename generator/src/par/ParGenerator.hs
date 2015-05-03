module ParGenerator where

import Ast
import qualified Data.Map as Map
import Data.List
import CmdGeneratorUtils
import Cmd
import Control.Monad.State

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


has_complex_call :: Exp -> FunctionGenerator Bool
has_complex_call e = do
  f <- lift $ gets CmdGeneratorUtils.is_complex
  return $ has_complex f e

is_complex_call :: Int -> FunctionGenerator Bool
is_complex_call id = do
  f <- lift $ gets CmdGeneratorUtils.is_complex
  return $ f id


handle_int :: Exp -> NeedType -> Bool -> FunctionGenerator ([CmdSeq], Bool)
handle_int e nt has_complex = do
    (bin, other) <- gather_binary e 
    (cmds, stricts) <- prepare_parameters other ToEnv has_complex
    need <- arith_need nt
    let non_strict = map (\x -> Wait $ ttarget $ fst x) $ filter (not.snd) $ zip other stricts in
      return (cmds ++ map no_label (non_strict ++ [Arith bin]) ++ need, True)

prepare_parameters :: [TargetedExp] -> (Int -> NeedType) -> Bool 
  -> FunctionGenerator ([CmdSeq], [Bool])
prepare_parameters [] _ _ = return ([], [])
prepare_parameters (h:t) nt complex_follows = do
  (cmds, stricts) <- prepare_parameters t nt complex_follows
  hc <- mapM (has_complex_call.texp) t
  (cms, strict) <- generate_exp (texp h) (nt $ ttarget h) (complex_follows || or hc) False
  return $ (cms ++ cmds, (strict:stricts))

handle_params :: [Exp] -> Cmd -> Cmd -> NeedType -> Bool -> FunctionGenerator [CmdSeq]
handle_params exps prep finalize nt complex_follows = do
    tparams <- mapM (\x -> alloc_local >>= \id -> return $ TargetedExp x id) exps
    cmds <- prepare_parameters tparams ToEnv complex_follows
    let pm = map (\e -> no_label $ PrepareParamMove (ttarget $ fst e) $ snd e) $ zip tparams [0..] in do
        fin <- acc_need nt
        return $ fst cmds ++ ([no_label prep] ++ pm ++ [no_label finalize]) ++ fin


generate_case :: CaseVariant -> NeedType -> Int -> Int -> Bool -> Bool 
  -> FunctionGenerator ([CmdSeq], Bool)
generate_case (CaseVariant f e) nt ml el complex_follows strict_expected = do
  let fields_move = map (\(fid, target) -> no_label $ StoreField fid target) $ zip [0..] f in do
    (ce, se) <- generate_exp e nt complex_follows strict_expected
    return ([CmdSeq (Just $ Label ml) Skip] ++ fields_move ++ ce ++ [no_label $ Jmp el], se)
    

--snd element of pair - info whether result expr is strict
generate_exp :: Exp -> NeedType -> Bool -> Bool ->  FunctionGenerator ([CmdSeq], Bool)
generate_exp e nt complex_follows strict_expected = case e of
  Ast.Construct id exps -> do
    c <- handle_params exps (AllocParams $ length exps) (Cmd.Construct id) nt complex_follows
    return (c, True)
  Ast.Operator _ _ _ -> handle_int e nt complex_follows
  Ast.Apply f p -> do
    id <- alloc_local
    nc <- has_complex_call f
    (pe, _) <- generate_exp p (ToEnv id) (nc || complex_follows) False
    (fe, _) <- generate_exp f Acc True True
    fin <- acc_need nt
    if not strict_expected && complex_follows
      then
        return $ (pe ++ fe ++ [no_label $ AddParamMoveParFork id] ++ fin, False)
      else
        return $ (pe ++ fe ++ [no_label $ AddParamMove id] ++ fin, True)
  Ast.Const _ -> handle_int e nt complex_follows 
  Ast.Local id -> do
    fin <- acc_need nt
    if strict_expected then
        return $ ([no_label $ Wait id, no_label $ Load id] ++ fin, True)
    else
        return $ ([no_label $ Load id] ++ fin, False)
  Ast.Global id p exps -> do
    cmds <- handle_params exps (AllocParams $ length exps) (Cmd.GlobalPar id) nt complex_follows
    return (cmds, True)
  Ast.Call id exps -> do
    id_complex  <- is_complex_call id
    if not strict_expected && complex_follows && id_complex
      then do
        cmds <- handle_params exps (AllocFunctionEnv id) (Cmd.CallFork id) nt True
        return (cmds, False)
      else do
        cmds <- handle_params exps (AllocFunctionEnv id) (Cmd.Call id) nt 
          (complex_follows || id_complex)
        return (cmds, True)
  Ast.Let id val ret -> do
    hc <- has_complex_call ret 
    (cval, _) <- generate_exp val (ToEnv id) (hc || complex_follows) False
    (cret, s) <- generate_exp ret nt complex_follows strict_expected
    return $ (cval ++ cret, s)
  Ast.If c t f -> do
    thc <- has_complex_call t
    fhc <- has_complex_call f
    (cc, _) <- generate_exp c ArithNeed (complex_follows || thc || fhc) True
    (ct, st) <- generate_exp t nt complex_follows strict_expected
    lf <- alloc_label
    (cf, sf) <- generate_exp f nt complex_follows strict_expected
    le <- alloc_label
    return $ (cc ++ [no_label $ JmpIfZero lf] ++ ct ++ [no_label $ Jmp le]
       ++ [CmdSeq (Just $ Label lf) Skip] ++  cf ++ [CmdSeq (Just $ Label le) Skip], 
       st && sf)
  Ast.Case value cases -> do
    hc <- mapM (\x -> has_complex_call $ case_exp x) cases
    (cval, _) <- generate_exp value Acc (complex_follows || or hc) True
    end_label <- alloc_label
    lcases <- mapM (\x -> alloc_label >>= \l -> return (x, l)) cases
    ccases <- mapM (\(mc, ml) -> generate_case mc nt ml end_label complex_follows strict_expected)
      lcases 
    return (cval ++ [no_label $ JmpCase $ map snd lcases] ++ (concat $ map fst ccases) 
      ++ [CmdSeq (Just $ Label end_label) Skip], and $ map snd ccases )

generate_function :: Ast.FunctionSpec -> Ast.Function -> SeqGenerator ([CmdSeq], FunctionCall)
generate_function spec (Ast.Function fid params locals exp) = do
  flable <- s_alloc_label
  ce <- runStateT (generate_exp exp Return False True) $ FunctionGeneratorState locals
  ace <- assign_apply_labels $ fst $ fst ce
  f <- gets CmdGeneratorUtils.is_complex
  return ((CmdSeq (Just $ NamedLabel flable $ Ast.fname spec) Skip:ace), 
    FunctionCall fid flable params (next_local $ snd ce) (Ast.fname spec) $ f fid)

generate_program :: Program -> SeqGenerator Cmds
generate_program prog = do
  cmds <- mapM (\(k, v) -> generate_function k v) $ Map.toList $ Ast.functions prog
  let calls =  Map.fromList $ map (\(_, x) -> (Cmd.fid x, x)) cmds in do
    final_label <- s_alloc_label
    case Map.lookup 0 calls of
      Just x -> return $ Cmds ((concat $ map fst cmds) ++ [CmdSeq (Just $ Label final_label) 
        Finalize]) calls x final_label
      Nothing -> sio $ ioError $ userError "Unknown function - should not happen" 

generate :: Program -> IO Cmds
generate p = let f = \k -> Map.findWithDefault False k $ complex_functions p in 
  evalStateT (generate_program p) (SeqGeneratorState 0 f)


