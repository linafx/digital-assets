{-# LANGUAGE OverloadedStrings #-}
module DA.Daml.LF.Simplifier2 (
    simplifyModule
) where

import Control.Monad.State.Strict
import DA.Daml.LF.Ast
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
    dsAnf <- NM.traverse (onBody anfExpr worldForAnf) (moduleValues m)
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
    { iFreeVars :: Set ExprVarName
    , iLambdas :: Map ExprVarName (Int, Expr) -- this is a subset of iFreeVars
    , iWorld :: World
    }

inlineExpr :: World -> Expr -> FreshM Expr
inlineExpr world e = do
    let env0 = InlineEnv
            { iFreeVars = Set.empty
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
        _ -> ELet <$> (Binding (x, t) <$> inline env e1) <*> inline (iIntroTmVar x env) e2
    ETmLam (x, t) e1 -> ETmLam (x, t) <$> inline (iIntroTmVar x env) e1
    ETyLam (t, k) e1 -> ETyLam (t, k) <$> inline env e1
    ECase e1 as -> do
        let handleAlt (CaseAlternative p e2) =
                CaseAlternative p <$> inline (iIntroTmVars (fvPattern p) env) e2
        ECase <$> inline env e1 <*> traverse handleAlt as
    ETmApp{} -> handleApp
    ETyApp{} -> handleApp
    _ -> pure e0
  where
    handleLetLam (Binding (x, t) e1) e2 = do
        e1' <- inline env e1
        let n = syntacticArity e1'
        ELet (Binding (x, t) e1') <$> inline (iIntroLambda x (n, e1') env) e2
    handleApp = do
        let (f, as) = takeEApps e0
        case f of
            EVar x
                | Just (n, e1) <- Map.lookup x (iLambdas env)
                , n == length as -- TODO(MH): Check that we never have more than `n` args.
                -> pure (mkEApps e1 as)
            _ -> pure e0


iIntroTmVar :: ExprVarName -> InlineEnv -> InlineEnv
iIntroTmVar x env = env{iFreeVars = Set.insert x (iFreeVars env)}

iIntroTmVars :: Set ExprVarName -> InlineEnv -> InlineEnv
iIntroTmVars xs env = env{iFreeVars = xs `Set.union` iFreeVars env}

iIntroLambda :: ExprVarName -> (Int, Expr) -> InlineEnv -> InlineEnv
iIntroLambda x lam env = env
    { iFreeVars = Set.insert x (iFreeVars env)
    , iLambdas = Map.insert x lam (iLambdas env)
    }

------------------------------------------------------------
-- ANF TRANSFORMATION
------------------------------------------------------------

data AnfEnv = AnfEnv
    { freeVars :: Map ExprVarName Int -- arity of the free variable
    , renamings :: Map ExprVarName ExprVarName
    , world :: World
    }

anfExpr :: World -> Expr -> FreshM Expr
anfExpr world e =
    let env0 = AnfEnv
            { freeVars = Map.empty
            , renamings = Map.empty
            , world = world
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
        let x' = Map.findWithDefault x x (renamings env) in
        let n = Map.findWithDefault 0 x' (freeVars env) in
        pure ([], EVar x', n)
    EVal q -> do
        let n = case lookupValue q (world env) of
                Left _ -> 0
                Right d -> runtimeArity (dvalBody d)
        pure ([], e, n)
    ETmLam (x, t) e0 -> do
        -- FIXME(MH): Rename `x` if it is bound already.
        (e1, n) <- anf (introVar x 0 env) e0
        pure ([], ETmLam (x, t) e1, n+1)
    ETyLam (t, k) e0 -> do
        (e1, n) <- anf env e0
        pure ([], ETyLam (t, k) e1, n)
    ELet (Binding (x, t) e1) e2 -> do
        (bs1, e1', n) <- anf' env e1
        (x', env') <- if x `Map.member` freeVars env
            then do
                x' <- freshTmVar
                pure (x', introRenaming x x' (introVar x' n env))
            else
                pure (x, introVar x n env)
        (bs2, e2', _) <- anf' env' e2
        pure (bs1 ++ [Binding (x', t) e1'] ++ bs2, e2', 0)
    ECase e0 as0 -> do
        anfAtomic env e0 $ \_ bs e1 _ -> do
            let anfAlt (CaseAlternative p e) = first (CaseAlternative p) <$> anf (introVars0 (fvPattern p) env) e
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
                (bs', e2, _) <- anf' (introVar x 0 env) (mkEApps (EVar x) zs1)
                pure (bs ++ [Binding (x, Nothing) e1] ++ bs', e2, 0)

anfAtomic :: AnfEnv -> Expr -> (AnfEnv -> [Binding] -> Expr -> Int -> FreshM a) -> FreshM a
anfAtomic env e0 cont = do
    (bs, e1, n) <- anf' env e0
    if isAtomic e1 then
        cont env bs e1 n
    else do
        x <- freshTmVar
        cont (introVar x n env) (bs ++ [Binding (x, Nothing) e1]) (EVar x) n

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

introVar :: ExprVarName -> Int -> AnfEnv -> AnfEnv
introVar x n env = env{freeVars = Map.insert x n (freeVars env)}

introVars0 :: Set ExprVarName -> AnfEnv -> AnfEnv
introVars0 xs env = env{freeVars = Map.fromSet (const 0) xs `Map.union` freeVars env}

introRenaming :: ExprVarName -> ExprVarName -> AnfEnv -> AnfEnv
introRenaming x y env = env{renamings = Map.insert x y (renamings env)}

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

fvPattern :: CasePattern -> Set ExprVarName
fvPattern p = case p of
    CPVariant _ _ x -> Set.singleton x
    CPEnum _ _ -> Set.empty
    CPUnit -> Set.empty
    CPBool _ -> Set.empty
    CPNil -> Set.empty
    CPCons x y -> Set.fromList [x, y]
    CPNone -> Set.empty
    CPSome x -> Set.singleton x
    CPDefault -> Set.empty

-- | Monadic version of mapAccumL
mapAccumLM :: forall t m acc x y. (Traversable t, Monad m) =>
    (acc -> x -> m (acc, y)) -> acc -> t x -> m (acc, t y)
mapAccumLM f z0 xs =
    let g x = StateT $ \z -> swap <$> f z x in
    swap <$> runStateT (traverse g xs) z0
