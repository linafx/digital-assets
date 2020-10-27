{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module DA.Daml.LF.Simplifier2 (
    simplifyModule
) where

import Control.Lens
import Control.Monad.State.Strict
import DA.Daml.LF.Ast
import qualified  DA.Daml.LF.Ast.Subst as Subst
import Data.Bifunctor
import Data.Functor.Foldable
import Data.Map.Strict (Map)
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
    dsAnf <- NM.traverse (onBody (anfExpr Map.empty) worldForAnf) (moduleValues m)
    m <- pure m{moduleValues = dsAnf}
    let worldForInline = extendWorldSelf m world
    dsInline <- NM.traverse (onBody inlineExpr worldForInline) (moduleValues m)
    m <- pure m{moduleValues = dsInline}
    pure m
  where
    onBody :: (World -> Expr -> FreshM Expr) -> World -> DefValue -> FreshM DefValue
    onBody f world d = do
        e <- f world (dvalBody d)
        pure d{dvalBody = e}

------------------------------------------------------------
-- INLINER
------------------------------------------------------------

data InlineEnv = InlineEnv
    { iBoundVars :: Set ExprVarName
    , iLambdas :: Map ExprVarName (Int, Expr) -- this is a subset of iBoundVars
    , iWorld :: World
    }

inlineExpr :: World -> Expr -> FreshM Expr
inlineExpr world e = do
    let env0 = InlineEnv
            { iBoundVars = Set.empty
            , iLambdas = Map.empty
            , iWorld = world
            }
    inline env0 e

inline :: InlineEnv -> Expr -> FreshM Expr
inline env e0 = case e0 of
    ELet b@(Binding (x, t) e1) e2 -> case e1 of
        EVar y
            | Just lam <- Map.lookup y (iLambdas env)
            -> ELet b <$> inline (iIntroLambda x lam env) e2
        EVal q
            | Right d <- lookupValue q (iWorld env)
            , let n = syntacticArity (dvalBody d)
            , n > 0
            -> ELet b <$> inline (iIntroLambda x (n, dvalBody d) env) e2
        ETyLam{} -> handleLetLam b e2
        ETmLam{} -> handleLetLam b e2
        EApp{} -> case handleApp e1 of
            Just e1' -> do
                let bvs = Map.map fst (iLambdas env) `Map.union` Map.fromSet (const 0) (iBoundVars env)
                -- TODO(MH): We don't need a full ANF transformation here but
                -- only the lifting of the outermost lets and the alpha renaming.
                e0' <- anfExpr bvs (iWorld env) (ELet (Binding (x, t) e1') e2)
                inline env e0'
            Nothing -> ELet b <$> inline (iIntroTmVar x env) e2
        _ -> ELet <$> (Binding (x, t) <$> inline env e1) <*> inline (iIntroTmVar x env) e2
    ETmLam (x, t) e1 -> ETmLam (x, t) <$> inline (iIntroTmVar x env) e1
    ETyLam (t, k) e1 -> ETyLam (t, k) <$> inline env e1
    ECase e1 as -> do
        let handleAlt (CaseAlternative p e2) =
                CaseAlternative p <$> inline (iIntroTmVars (patternVars p) env) e2
        ECase <$> inline env e1 <*> traverse handleAlt as
    EApp{}
        | Just e0' <- handleApp e0 -> do
            let bvs = Map.map fst (iLambdas env) `Map.union` Map.fromSet (const 0) (iBoundVars env)
            -- TODO(MH): We don't need a full ANF transformation here but
            -- only the alpha renaming.
            e0'' <- anfExpr bvs (iWorld env) e0'
            inline env e0''
    _ -> pure e0
  where
    handleLetLam (Binding (x, t) e1) e2 = do
        e1' <- inline env e1
        let n = syntacticArity e1'
        ELet (Binding (x, t) e1') <$> inline (iIntroLambda x (n, e1') env) e2
    handleApp e0 =
        let (f, as) = takeEApps e0 in
        case f of
            EVar x
                | Just (n, e1) <- Map.lookup x (iLambdas env)
                , n == length as -- TODO(MH): Check that we never have more than `n` args.
                -> Just (apply e1 as)
            _ -> Nothing
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

iIntroTmVar :: ExprVarName -> InlineEnv -> InlineEnv
iIntroTmVar x env = env{iBoundVars = Set.insert x (iBoundVars env)}

iIntroTmVars :: Set ExprVarName -> InlineEnv -> InlineEnv
iIntroTmVars xs env = env{iBoundVars = xs `Set.union` iBoundVars env}

iIntroLambda :: ExprVarName -> (Int, Expr) -> InlineEnv -> InlineEnv
iIntroLambda x lam env = env
    { iBoundVars = Set.insert x (iBoundVars env)
    , iLambdas = Map.insert x lam (iLambdas env)
    }

------------------------------------------------------------
-- ANF TRANSFORMATION
------------------------------------------------------------

data AnfEnv = AnfEnv
    { aBoundVars :: Map ExprVarName Int -- arity of the free variable
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
    ETyLam{} -> syntacticArity e0
    ETmLam{} -> syntacticArity e0
    ELet _ e1 -> runtimeArity e1
    ELocation _ e1 -> runtimeArity e1
    -- TODO(MH): Take partially applied top-level values into account
    _ -> 0

-- | Number of leading lambdas of the expression.
syntacticArity :: Expr -> Int
syntacticArity e0 = case e0 of
    ETmLam _ e1 -> syntacticArity e1 + 1
    ETyLam _ e1 -> syntacticArity e1 + 1
    ELocation _ e1 -> syntacticArity e1
    _ -> 0

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

-- | Monadic version of mapAccumL
mapAccumLM :: forall t m acc x y. (Traversable t, Monad m) =>
    (acc -> x -> m (acc, y)) -> acc -> t x -> m (acc, t y)
mapAccumLM f z0 xs =
    let g x = StateT $ \z -> swap <$> f z x in
    swap <$> runStateT (traverse g xs) z0
