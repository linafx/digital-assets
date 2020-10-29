{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module DA.Daml.LF.Simplifier2 (
    simplifyModule
) where

import Control.Lens
import Control.Monad.State.Strict
import DA.Daml.LF.Ast
import qualified DA.Daml.LF.Ast.Subst as Subst
import qualified DA.Daml.LF.TypeChecker.Check as Check
import DA.Pretty (renderPretty)
import Data.Bifunctor
import Data.Functor.Foldable
import Data.List.Extra
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Tuple (swap)
import qualified Data.Map.Strict as Map
import qualified Data.NameMap as NM
import qualified Data.Set as Set
import qualified Data.Text.Extended as T

data FreshState = FreshState
    { tmVarCounter :: Int
    }

newtype FreshM a = FreshM{unFreshM :: State FreshState a}
    deriving (Functor, Applicative, Monad)

runFreshM :: FreshM a -> a
runFreshM act = evalState (unFreshM act) FreshState
    { tmVarCounter = 0
    }

freshTmVar :: FreshM ExprVarName
freshTmVar = FreshM $ do
    n <- state (\st -> let k = tmVarCounter st + 1 in (k, st{tmVarCounter = k}))
    pure (ExprVarName ("$v" <> T.show n))

simplifyModule :: World -> Version -> Module -> Module
simplifyModule world _version m = runFreshM $ do
    let worldForAnf = extendWorldSelf m world
    m <- onBodies (anfExpr Map.empty worldForAnf) m
    let worldForInline = extendWorldSelf m world
    m <- onBodies (inlineExpr worldForInline) m
    let worldForClean = extendWorldSelf m world
    m <- onBodies (pure . cleanExpr worldForClean) m
    pure m
  where
    onBodies :: (Expr -> FreshM Expr) -> Module -> FreshM Module
    onBodies f m = do
        let fOnDefValue d@DefValue{dvalBody = e} = do
            e' <- case e of
                -- NOTE(MH): This is to keep type class dictionaries detectable.
                EStructCon fes -> EStructCon <$> traverse (\(x, e) -> (,) x <$> f e) fes
                _ -> f e
            pure d{dvalBody = e'}
        ds <- NM.traverse fOnDefValue (moduleValues m)
        pure m{moduleValues = ds}

------------------------------------------------------------
-- CLEANER
------------------------------------------------------------

data CleanEnv = CleanEnv
    { cBoundVars :: Map ExprVarName Int  -- runtime arity of the variable
    , cInlinings :: Map ExprVarName Expr  -- all expressions are atomic
    , cWorld :: World
    }

cleanExpr :: World -> Expr -> Expr
cleanExpr world e0 =
    let env0 = CleanEnv
            { cBoundVars = Map.empty
            , cInlinings = Map.empty
            , cWorld = world
            }
    in
    let (e1, _, _, _) = clean env0 e0 in
    e1

-- Int is runtime arity, Bool says if this might have effects
clean :: CleanEnv -> Expr -> (Expr, Set ExprVarName, Int, Bool)
clean env e0 = case e0 of
    EVar x ->
        let e0' = Map.findWithDefault e0 x (cInlinings env) in
        let (fvs, n) = case e0' of
                EVar x' -> (Set.singleton x', cLookupTmVar x' env)
                EBuiltin b -> (Set.empty, builtinArity b)
                ENil{} -> (Set.empty, 0)
                ENone{} -> (Set.empty, 0)
                _ -> error "cInlinings returned non-atom"
        in
        (e0', fvs, n, False)
    EBuiltin b -> (e0, Set.empty, builtinArity b, False)
    ELet (Binding (x, _) (EVar y)) e2 ->
        let e1' = Map.findWithDefault (EVar y) y (cInlinings env) in
        clean (cIntroInlining x e1' env) e2
    ELet (Binding (x, _) e1) e2 | isAtomic e1 ->
        clean (cIntroInlining x e1 env) e2
    ELet (Binding (x, t) e1) e2 ->
        let (e1', fvs1, n1, b1) = clean env e1 in
        let (e2', fvs2, n2, b2) = clean (cIntroTmVar x n1 env) e2 in
        if e2' == EVar x then
            (e1', fvs1, n1, b1)
        else if b1 || x `Set.member` fvs2 then
            (ELet (Binding (x, t) e1') e2', fvs1 `Set.union` Set.delete x fvs2, n2, b1 || b2)
        else
            (e2', fvs2, n2, b2)
    EVal q -> case lookupValue q (cWorld env) of
        Left _ ->
            (e0, Set.empty, 0, True)
        Right DefValue{dvalBody = b} ->
            (e0, Set.empty, runtimeArity b, hasEffect b)
    ETmLam (x, t) e1 ->
        let (e1', fvs1, n1, _) = clean (cIntroTmVar x 0 env) e1 in
        let n0' = case e1' of
                ELam{} -> n1 + 1
                _ -> 1
        in
        (ETmLam (x, t) e1', Set.delete x fvs1, n0', False)
    ETyLam (t, k) e1 ->
        let (e1', fvs1, n1, _) = clean env e1 in
        let n0' = case e1' of
                ELam{} -> n1 + 1
                _ -> 1
        in
        (ETyLam (t, k) e1', fvs1, n0', False)
    EApp{} ->
        let (f, as) = takeEApps e0 in
        let (f', fvs_f, n_f, eff_f) = clean env f in
        let (as', fvs_as, eff_as) = cleanArgs env as in
        let n = max 0 (n_f - length as) in
        (mkEApps f' as', fvs_f `Set.union` fvs_as, n, eff_f || eff_as || n == 0)
    ECase e1 as ->
        let (e1', fvs1, _, eff_e1) = clean env e1 in
        let handleAlt (CaseAlternative p e) =
                let pvs = patternVars p in
                let (e', fvs, n, b) = clean (cIntroTmVars0 pvs env) e in
                (CaseAlternative p e', fvs `Set.difference` pvs, n, b)
        in
        let (as', fvss, ns, eff_as) = unzip4 (map handleAlt as) in
        (ECase e1' as', fvs1 `Set.union` Set.unions fvss, minimum (1_000_000:ns), eff_e1 || or eff_as)
    _ ->
        let info = fmap (clean env) (project e0) in
        let e0' = embed (fmap (\(e', _, _, _) -> e') info) in
        let fvs = Set.unions (fmap (\(_, fvs, _, _) -> fvs) info) in
        let effs = or (fmap (\(_, _, _, eff) -> eff) info) in
        (e0', fvs, 0, effs)

cleanArgs :: CleanEnv -> [Arg] -> ([Arg], Set ExprVarName, Bool)
cleanArgs env = \case
    [] -> ([], Set.empty, False)
    TyArg t:as ->
        let (as', fvs, eff) = cleanArgs env as in
        (TyArg t:as', fvs, eff)
    TmArg e:as ->
        let (e', fvs_e, _, eff_e) = clean env e in
        let (as', fvs_as, eff_as) = cleanArgs env as in
        (TmArg e':as', fvs_e `Set.union` fvs_as, eff_e || eff_as)

cIntroTmVar :: ExprVarName -> Int -> CleanEnv -> CleanEnv
cIntroTmVar x n env = env{cBoundVars = Map.insert x n (cBoundVars env)}

cLookupTmVar :: ExprVarName -> CleanEnv -> Int
cLookupTmVar x env = case Map.lookup x (cBoundVars env) of
    Just n -> n
    Nothing -> error $ "reference to unbound variable " ++ show x

cIntroTmVars0 :: Set ExprVarName -> CleanEnv -> CleanEnv
cIntroTmVars0 xs env = env{cBoundVars = Map.fromSet (const 0) xs `Map.union` cBoundVars env}

cIntroInlining :: ExprVarName -> Expr -> CleanEnv -> CleanEnv
cIntroInlining x e env = env{cInlinings = Map.insert x e (cInlinings env)}

hasEffect :: Expr -> Bool
hasEffect e = case e of
    ELam{} -> False
    EBuiltin{} -> False
    EStructCon fes
        | all (\(_, e) -> syntacticArity e > 0) fes -> False
    _ -> True  -- TODO(MH): This is _very_ conservative.

------------------------------------------------------------
-- INLINER
------------------------------------------------------------

data Atom
    = AVar ExprVarName
    | ABuiltin BuiltinExpr
    | ANone Type
    | ANil Type

data Value
    = VAbstract
    | VPAP BuiltinExpr [Arg]
    | VLam Expr
    | VPALam ExprVarName Expr [Arg]
    | VRecord [(FieldName, Atom)]
    | VStruct [(FieldName, Atom)]
    | VVariant VariantConName Atom
    | VEnum VariantConName
    | VNone
    | VSome Atom
    | VNil
    | VCons Atom Atom
    | VDict [(FieldName, Expr)]
    | VMethod Expr
    | Whnf Expr

pattern VBuiltin :: BuiltinExpr -> Value
pattern VBuiltin b = VPAP b []

valueArity :: Value -> Int
valueArity = \case
    VAbstract -> 0
    VPAP b as -> builtinArity b - length as
    VLam e -> syntacticArity e
    VPALam _ e as -> syntacticArity e - length as
    VRecord{} -> 0
    VStruct{} -> 0
    VVariant{} -> 0
    VEnum{} -> 0
    VNone -> 0
    VSome{} -> 0
    VNil -> 0
    VCons{} -> 0
    VDict{} -> 0
    VMethod e -> syntacticArity e
    Whnf e -> syntacticArity e

data InlineEnv = InlineEnv
    { iBoundVars :: Map ExprVarName Value
    , iWorld :: World
    }

inlineExpr :: World -> Expr -> FreshM Expr
inlineExpr world e = do
    let env0 = InlineEnv
            { iBoundVars = Map.empty
            , iWorld = world
            }
    inline env0 e

inline :: InlineEnv -> Expr -> FreshM Expr
inline env e0 = case e0 of
    ELet (Binding (x, t) e1) e2 -> do
        info <- evaluate env e1
        case info of
            Right (e1', v) -> do
                ELet (Binding (x, t) e1') <$> inline (iIntroTmVar x v env) e2
            Left e1' -> do
                let bvs = Map.map valueArity (iBoundVars env)
                -- TODO(MH): We might notneed a full ANF transformation here but
                -- only the lifting of the outermost lets and the alpha renaming.
                e0' <- anfExpr bvs (iWorld env) (ELet (Binding (x, t) e1') e2)
                inline env e0'
    _ -> do
        info <- evaluate env e0
        case info of
            Right (e0', _) -> pure e0'
            Left e0' -> do
                let bvs = Map.map valueArity (iBoundVars env)
                -- TODO(MH): We might notneed a full ANF transformation here but
                -- only the alpha renaming.
                e0'' <- anfExpr bvs (iWorld env) e0'
                inline env e0''

-- Left means we need to re-normalize. Expr in Right is necessary because we
-- need to inline subexpressions of lambdas and case expressions.
evaluate :: InlineEnv -> Expr -> FreshM (Either Expr (Expr, Value))
evaluate env e0 = case e0 of
    -- NOTE(MH): For type classes we're happy to inline the method as soon as
    -- we have it. It will either be a reference to another function or a lambda
    -- containing getField or setField.
    -- BEGIN special treatment for type classes.
    EStructProj f (lookupAsAtom env -> VDict fes) ->
        pure $ Right (e0, VMethod (fromJust (f `lookup` fes)))
    ETmApp (EVar x) EUnit
        | VMethod m <- iLookupTmVar x env -> pure $ Left m
    -- END special treatment for type classes.
    EVar x -> pure $ Right (e0, iLookupTmVar x env)
    EBuiltin b -> pure $ Right (e0, VBuiltin b)
    EVal q -> case lookupValue q (iWorld env) of
        Left _ -> pureAbstract e0
        Right d -> do
            let v = case dvalBody d of
                    e@ELam{} -> VLam e
                    EStructCon fes
                        | Just dict <- traverse isMethod fes -> VDict dict
                      where
                        isMethod (f, e) = case e of
                            ETmLam (_, TUnit) e' -> Just (f, e')
                            _ -> Nothing
                    _ -> VAbstract
            pure $ Right (e0, v)
    ETyLam (t, k) e1 -> do
        e0' <- ETyLam (t, k) <$> inline env e1
        pure $ Right (e0', VLam e0')
    ETmLam (x, t) e1 -> do
        e0' <- ETmLam (x, t) <$> inline (iIntroTmVar x VAbstract env) e1
        pure $ Right (e0', VLam e0')
    ETmApp{} -> handleApp e0
    ETyApp{} -> handleApp e0
    ELet{} -> error "let under let"
    ECase e1 as -> handleCase e1 as
    ERecCon _ fas -> do
        let v = VRecord [(f, asAtom a) | (f, a) <- fas]
        pure $ Right (e0, v)
    ERecProj _ f a
        | VRecord fes <- lookupAsAtom env a -> do
            let a' = fromJust (lookup f fes)
            pure $ Right (asExpr a', lookupAtom env a')
        | otherwise -> pureAbstract e0
    ERecUpd{} -> pureAbstract e0  -- TODO(MH): Implement.
    EStructCon fas -> do
        let v = VRecord [(f, asAtom a) | (f, a) <- fas]
        pure $ Right (e0, v)
    EStructProj f a
        | VStruct fes <- lookupAsAtom env a -> do
            let a' = fromJust (lookup f fes)
            pure $ Right (asExpr a', lookupAtom env a')
        | otherwise -> pureAbstract e0
    EStructUpd{} -> pureAbstract e0  -- TODO(MH): Implement.
    EVariantCon _ c a -> do
        let v = VVariant c (asAtom a)
        pure $ Right (e0, v)
    EEnumCon _ c -> pure $ Right (e0, VEnum c)
    ENil _ -> pure $ Right (e0, VNil)
    ECons _ a1 a2 -> do
        let v = VCons (asAtom a1) (asAtom a2)
        pure $ Right (e0, v)
    ENone{} -> pure $ Right (e0, VNone)
    ESome _ a -> do
        let v = VSome (asAtom a)
        pure $ Right (e0, v)
    ELocation _ e1 -> evaluate env e1
    -- TODO(MH): Implement the builtins below.
    EToAny{} -> pureAbstract e0
    EFromAny{} -> pureAbstract e0
    ETypeRep{} -> pureAbstract e0
    EUpdate{} -> pureAbstract e0
    EScenario{} -> pureAbstract e0
  where
    pureAbstract e = pure $ Right (e, VAbstract)
    handleApp e0 = do
        let (f, as) = takeEApps e0
        case f of
            EVar x -> case iLookupTmVar x env of
                VAbstract -> pureAbstract e0
                VLam e -> case syntacticArity e `compare` length as of
                    LT -> error "overapplied lambda"
                    EQ -> pure $ Left (apply e as)
                    GT -> pure $ Right (e0, VPALam x e as)
                VPALam y e as' -> case syntacticArity e `compare` (length as' + length as) of
                    LT -> error "overapplied lambda"
                    EQ -> pure $ Left (apply e (as' ++ as))
                    GT -> pure $ Right (e0, VPALam y e (as' ++ as))
                VPAP b as' -> case builtinArity b `compare` (length as' + length as) of
                    LT -> error "overapplied builtin"
                    EQ -> pureAbstract $ mkEApps (EBuiltin b) (as' ++ as)
                    GT -> pure $ Right (e0, VPAP b (as' ++ as))
                _ -> illTyped
            EBuiltin b -> case builtinArity b `compare` length as of
                LT -> error "overapplied builtin"
                EQ -> pureAbstract e0
                GT -> pure $ Right (e0, VPAP b as)
            _ -> illTyped
      where
        illTyped = error $ "ill-typed appplication in " ++ renderPretty e0
    handleCase e1 as = do
        case normalize env e1 of
            VAbstract -> do
                let handleAlt (CaseAlternative p e2) =
                        CaseAlternative p <$> inline (iIntroAbstractTmVars (patternVars p) env) e2
                e0' <- ECase <$> inline env e1 <*> traverse handleAlt as
                pureAbstract e0'
            VBuiltin BEUnit -> case firstJust match as of
                Nothing -> nonExhaustive
                Just a -> pure $ Left a
              where
                match (CaseAlternative p e) = case p of
                    CPUnit -> Just e
                    CPDefault -> Just e
                    _ -> Nothing
            VBuiltin (BEBool v) -> case firstJust match as of
                Nothing -> nonExhaustive
                Just a -> pure $ Left a
              where
                match (CaseAlternative p e) = case p of
                    CPBool b | b == v -> Just e
                    CPDefault -> Just e
                    _ -> Nothing
            VNone -> case firstJust match as of
                Nothing -> nonExhaustive
                Just a -> pure $ Left a
              where
                match (CaseAlternative p e) = case p of
                    CPNone -> Just e
                    CPDefault -> Just e
                    _ -> Nothing
            VSome v -> case firstJust match as of
                Nothing -> nonExhaustive
                Just a -> pure $ Left a
              where
                match (CaseAlternative p e) = case p of
                    CPSome x -> Just (ELet (Binding (x, Nothing) (asExpr v)) e)
                    CPDefault -> Just e
                    _ -> Nothing
            VNil -> case firstJust match as of
                Nothing -> nonExhaustive
                Just a -> pure $ Left a
              where
                match (CaseAlternative p e) = case p of
                    CPNil -> Just e
                    CPDefault -> Just e
                    _ -> Nothing
            VCons h t -> case firstJust match as of
                Nothing -> nonExhaustive
                Just a -> pure $ Left a
              where
                match (CaseAlternative p e) = case p of
                    CPCons x y -> Just (ELet (Binding (x, Nothing) (asExpr h)) $ ELet (Binding (y, Nothing) (asExpr t)) e)
                    CPDefault -> Just e
                    _ -> Nothing
            VVariant c v -> case firstJust match as of
                Nothing -> nonExhaustive
                Just a -> pure $ Left a
              where
                match (CaseAlternative p e) = case p of
                    CPVariant _ c' x | c == c' -> Just (ELet (Binding (x, Nothing) (asExpr v)) e)
                    CPDefault -> Just e
                    _ -> Nothing
            VEnum c -> case firstJust match as of
                Nothing -> nonExhaustive
                Just a -> pure $ Left a
              where
                match (CaseAlternative p e) = case p of
                    CPEnum _ c' | c == c' -> Just e
                    CPDefault -> Just e
                    _ -> Nothing
            _ -> illTyped
          where
            e0 = ECase e1 as
            nonExhaustive = error $ "non-exhaustive pattern match in " ++ renderPretty e0
            illTyped = error $ "pattern match on non-metchable type in " ++ renderPretty e0

apply :: Expr -> [Arg] -> Expr
apply e0 as = case as of
    [] -> e0
    TmArg e1:as -> case e0 of
        ETmLam (x, t) e2 -> ELet (Binding (x, Just t) e1) (apply e2 as)
        _ -> error $ "type or arity error: applying expr " ++ show e1 ++ " to " ++ show e0
    TyArg t1:as -> case e0 of
        -- TODO(MH): Repeated substitution is not efficient.
        ETyLam (v, _) e1 -> apply (Subst.applySubstInExpr (Subst.typeSubst v t1) e1) as
        _ -> error $ "type or arity error: applying type " ++ show t1 ++ " to " ++ show e0

normalizeTmVar :: InlineEnv -> ExprVarName -> Value
normalizeTmVar env x = case iLookupTmVar x env of
    Whnf (EVar x) -> normalizeTmVar env x
    v -> v

normalize :: InlineEnv -> Expr -> Value
normalize env = \case
    EVar x -> normalizeTmVar env x
    e -> Whnf e

lookupAsAtom :: InlineEnv -> Expr -> Value
lookupAsAtom env = lookupAtom env . asAtom

lookupAtom :: InlineEnv -> Atom -> Value
lookupAtom env = \case
    AVar x -> iLookupTmVar x env
    ABuiltin b -> VBuiltin b
    ANone _ -> VNone
    ANil _ -> VNil

asAtom :: Expr -> Atom
asAtom = \case
    EVar x -> AVar x
    EBuiltin b -> ABuiltin b
    ENone t -> ANone t
    ENil t -> ANil t
    e -> error $ "expected atom, found " ++ renderPretty e

asExpr :: Atom -> Expr
asExpr = \case
    AVar x -> EVar x
    ABuiltin b -> EBuiltin b
    ANone t -> ENone t
    ANil t -> ENil t

iLookupTmVar :: ExprVarName -> InlineEnv -> Value
iLookupTmVar x env = case Map.lookup x (iBoundVars env) of
    Just v -> v
    Nothing -> error ("reference to unbound variable " ++ renderPretty x)

iIntroTmVar :: ExprVarName -> Value -> InlineEnv -> InlineEnv
iIntroTmVar x info env = env{iBoundVars = Map.insert x info (iBoundVars env)}

iIntroAbstractTmVars :: Set ExprVarName -> InlineEnv -> InlineEnv
iIntroAbstractTmVars xs env = env{iBoundVars = Map.fromSet (const VAbstract) xs `Map.union` iBoundVars env}

------------------------------------------------------------
-- ANF TRANSFORMATION
------------------------------------------------------------

data AnfEnv = AnfEnv
    { aBoundVars :: Map ExprVarName Int -- runtime arity of the free variable
    , aRenamings :: Map ExprVarName ExprVarName
    , aWorld :: World
    }

anfExpr :: Map ExprVarName Int -> World -> Expr -> FreshM Expr
anfExpr bvs world e =
    let env0 = AnfEnv
            { aBoundVars = bvs
            , aRenamings = Map.empty
            , aWorld = world
            }
    in
    fst <$> anf env0 e

anf :: AnfEnv -> Expr -> FreshM (Expr, Int)
anf env e0 = do
    (bs, e1, n) <- anf' env e0
    pure (mkELets bs e1, n)

anf' :: AnfEnv -> Expr -> FreshM ([Binding], Expr, Int)
anf' env e = case e of
    EVar x ->
        let x' = Map.findWithDefault x x (aRenamings env) in
        let n = Map.findWithDefault 0 x' (aBoundVars env) in
        pure ([], EVar x', n)
    EVal q -> do
        let n = case lookupValue q (aWorld env) of
                Left _ -> 0
                Right d -> runtimeArity (dvalBody d)
        pure ([], e, n)
    ETmLam (x, t) e0 -> aIntroTmVar env x 0 $ \env x -> do
        (e1, n) <- anf env e0
        pure ([], ETmLam (x, t) e1, n+1)
    ETyLam (t, k) e0 -> do
        (e1, n) <- anf env e0
        pure ([], ETyLam (t, k) e1, n)
    ELet (Binding (x, t) e1) e2 -> do
        (bs1, e1', n) <- anf' env e1
        aIntroTmVar env x n $ \env x -> do
            (bs2, e2', _) <- anf' env e2
            pure (bs1 ++ [Binding (x, t) e1'] ++ bs2, e2', 0)
    ECase e0 as0 -> do
        anfAtomic env e0 $ \_ bs e1 _ -> do
            let anfAlt (CaseAlternative p e) =
                    aIntroPattern env p $ \env p -> first (CaseAlternative p) <$> anf env e
            (as1, ns) <- unzip <$> traverse anfAlt as0
            pure (bs, ECase e1 as1, if null ns then 0 else minimum ns)
    ETyApp{} -> handleApp
    ETmApp{} -> handleApp
    EUpdate{} -> pure ([], e, 0)
    EScenario{} -> pure ([], e, 0)
    ELocation _ e' -> anf' env e'
    EBuiltin{} -> defaultAnf
    ERecCon{} -> defaultAnf
    ERecProj{} -> defaultAnf
    ERecUpd{} -> defaultAnf
    EVariantCon{} -> defaultAnf
    EEnumCon{} -> defaultAnf
    EStructCon{} -> defaultAnf
    EStructProj{} -> defaultAnf
    EStructUpd{} -> defaultAnf
    ENil{} -> defaultAnf
    ECons{} -> defaultAnf
    ESome{} -> defaultAnf
    ENone{} -> defaultAnf
    EToAny{} -> defaultAnf
    EFromAny{} -> defaultAnf
    ETypeRep{} -> defaultAnf
  where
    defaultAnf = do
        (bs, e') <- anfMany env (project e)
        pure (bs, embed e', 0)
    handleApp = do
        let (f0, as0) = takeEApps e
        anfAtomic env f0 $ \_ bsf f1 n -> do
            let k = max 1 n -- we must consume at least one argument, otherwise we'll loop
            let (as1, zs1) = splitAt k as0
            (bsas, as2) <- anfArgs env as1
            let (bs, e1) = (bsf ++ bsas, mkEApps f1 as2)
            if null zs1 then
                pure (bs, e1, k - length as1)
            else do
                x <- freshTmVar
                aIntroTmVar env x 0 $ \env x -> do
                    (bs', e2, _) <- anf' env (mkEApps (EVar x) zs1)
                    pure (bs ++ [Binding (x, Nothing) e1] ++ bs', e2, 0)

anfAtomic :: AnfEnv -> Expr -> (AnfEnv -> [Binding] -> Expr -> Int -> FreshM a) -> FreshM a
anfAtomic env e0 cont = do
    (bs, e1, n) <- anf' env e0
    if isAtomic e1 then
        cont env bs e1 n
    else do
        x <- freshTmVar
        aIntroTmVar env x n $ \env x -> do
        cont env (bs ++ [Binding (x, Nothing) e1]) (EVar x) n

anfMany :: Traversable t => AnfEnv -> t Expr -> FreshM ([Binding], t Expr)
anfMany env e0s = do
    let step env e0 = anfAtomic env e0 $ \env bs e1 _ -> pure (env, (bs, e1))
    (_, bse1s) <- mapAccumLM step env e0s
    pure (concatMap fst bse1s, fmap snd bse1s)

anfArgs :: AnfEnv -> [Arg] -> FreshM ([Binding], [Arg])
anfArgs env a0s = do
    let step env a0 = case a0 of
            TmArg e0 -> anfAtomic env e0 $ \env bs e1 _ -> pure (env, (bs, TmArg e1))
            TyArg{} -> pure (env, ([], a0))
    (_, bsa1s) <- mapAccumLM step env a0s
    pure (concatMap fst bsa1s, fmap snd bsa1s)

aIntroTmVar :: AnfEnv -> ExprVarName -> Int -> (AnfEnv -> ExprVarName -> FreshM a) -> FreshM a
aIntroTmVar env x n cont
    | x `Map.member` aBoundVars env = do
        y <- freshTmVar
        let env' = env
                { aBoundVars = Map.insert y n (aBoundVars env)
                , aRenamings = Map.insert x y (aRenamings env)
                }
        cont env' y
    | otherwise = do
        let env' = env{aBoundVars = Map.insert x n (aBoundVars env)}
        cont env' x

aIntroPattern :: AnfEnv -> CasePattern -> (AnfEnv -> CasePattern -> FreshM a) -> FreshM a
aIntroPattern env p cont = case p of
    CPVariant t c x -> aIntroTmVar env x 0 $ \env x -> cont env (CPVariant t c x)
    CPEnum{} -> cont env p
    CPUnit{} -> cont env p
    CPBool{} -> cont env p
    CPNil -> cont env p
    CPCons x y -> aIntroTmVar env x 0 $ \env x -> aIntroTmVar env y 0 $ \env y -> cont env (CPCons x y)
    CPNone -> cont env p
    CPSome x -> aIntroTmVar env x 0 $ \env x -> cont env (CPSome x)
    CPDefault -> cont env p

isAtomic :: Expr -> Bool
isAtomic e = case e of
    EVar{} -> True
    EBuiltin{} -> True
    ENil{} -> True
    ENone{} -> True
    _ -> False

------------------------------------------------------------
-- UTILITIES
------------------------------------------------------------

-- | Lower bound on the arity of the expression once it has been evaluated.
runtimeArity :: Expr -> Int
runtimeArity e0 = case e0 of
    ELam{} -> syntacticArity e0
    EBuiltin b -> builtinArity b
    ELet _ e1 -> runtimeArity e1
    ELocation _ e1 -> runtimeArity e1
    -- TODO(MH): Take partially applied top-level values and builtins into account
    _ -> 0

-- | Number of leading lambdas of the expression.
syntacticArity :: Expr -> Int
syntacticArity e0 = case e0 of
    ELam _ e1 -> syntacticArity e1 + 1
    ELocation _ e1 -> syntacticArity e1
    _ -> 0

-- | Arity of the builtin, including the type abstractions.
builtinArity :: BuiltinExpr -> Int
builtinArity b =
    let (vs, t) = takeTForalls (Check.typeOfBuiltin b) in
    let (ps, _) = takeTFuns t in
    length vs + length ps

patternVars :: CasePattern -> Set ExprVarName
patternVars p = case p of
    CPVariant _ _ x -> Set.singleton x
    CPEnum _ _ -> Set.empty
    CPUnit -> Set.empty
    CPBool _ -> Set.empty
    CPNil -> Set.empty
    CPCons x y -> Set.fromList [x, y]
    CPNone -> Set.empty
    CPSome x -> Set.singleton x
    CPDefault -> Set.empty

pattern EApp :: Expr -> Arg -> Expr
pattern EApp fun arg <- (matching _EApp -> Right (fun, arg))
  where
    EApp fun arg = mkEApp fun arg

-- pattern EApps :: Expr -> [Arg] -> Expr
-- pattern EApps f as <- (takeEApps -> (f, as))
--   where
--     EApps f as = mkEApps f as

data Lam
    = TmLam (ExprVarName, Type)
    | TyLam (TypeVarName, Kind)

pattern ELam :: Lam -> Expr -> Expr
pattern ELam lam body <- (takeELam -> Just (lam, body))
  where
    ELam lam body = mkELam lam body

mkELam :: Lam -> Expr -> Expr
mkELam l e = case l of
    TmLam b -> ETmLam b e
    TyLam b -> ETyLam b e

takeELam :: Expr -> Maybe (Lam, Expr)
takeELam e0 = case e0 of
    ETmLam b e1 -> Just (TmLam b, e1)
    ETyLam b e1 -> Just (TyLam b, e1)
    _ -> Nothing

-- takeELams :: Expr -> ([Lam], Expr)
-- takeELams = \case
--     ELam b e -> first (b:) (takeELams e)
--     e -> ([], e)

takeTFuns :: Type -> ([Type], Type)
takeTFuns = \case
    a :-> b -> first (a:) (takeTFuns b)
    t -> ([], t)

takeTForalls :: Type -> ([(TypeVarName, Kind)], Type)
takeTForalls = \case
    TForall v t -> first (v:) (takeTForalls t)
    t -> ([], t)

-- mkELams :: [Lam] -> Expr -> Expr
-- mkELams ls e = foldr mkELam e ls

-- takeELams :: Expr -> ([Lam], Expr)
-- takeELams e0 = case e0 of
--     ETmLam b e1 -> first (TmLam b:) (takeELams e1)
--     ETyLam b e1 -> first (TyLam b:) (takeELams e1)
--     ELocation _ e1 -> takeELams e1
--     _ -> ([], e0)

-- | Monadic version of mapAccumL
mapAccumLM :: forall t m acc x y. (Traversable t, Monad m) =>
    (acc -> x -> m (acc, y)) -> acc -> t x -> m (acc, t y)
mapAccumLM f z0 xs =
    let g x = StateT $ \z -> swap <$> f z x in
    swap <$> runStateT (traverse g xs) z0
