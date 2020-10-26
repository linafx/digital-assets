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

data AnfEnv = AnfEnv
    { freeVars :: Map ExprVarName Int -- arity of the free variable
    , renamings :: Map ExprVarName ExprVarName
    , world :: World
    }

data AnfState = AnfState
    { freshVarCounter :: Int
    }

type AnfM = State AnfState

simplifyModule :: World -> Version -> Module -> Module
simplifyModule world version m =
    let world' = extendWorldSelf m world in
    m{moduleValues = NM.map (simplifyDefValue world' version) (moduleValues m)}

simplifyDefValue :: World -> Version -> DefValue -> DefValue
simplifyDefValue world version d = d{dvalBody = simplifyExpr world version (dvalBody d)}

simplifyExpr :: World -> Version -> Expr -> Expr
simplifyExpr world _ e =
    let env0 = AnfEnv
            { freeVars = Map.empty
            , renamings = Map.empty
            , world = world
            }
    in
    fst (runAnfM (anf env0 e))


runAnfM :: AnfM a -> a
runAnfM act = evalState act AnfState
    { freshVarCounter = 0
    }

anf :: AnfEnv -> Expr -> AnfM (Expr, Int)
anf env e0 = do
    (bs, e1, n) <- anf' env e0
    pure (mkELets bs e1, n)

anf' :: AnfEnv -> Expr -> AnfM ([Binding], Expr, Int)
anf' env e = case e of
    EVar x ->
        let x' = Map.findWithDefault x x (renamings env) in
        let n = Map.findWithDefault 0 x' (freeVars env) in
        pure ([], EVar x', n)
    EVal q -> do
        let n = case lookupValue q (world env) of
                Left _ -> 0
                Right d -> arity (dvalBody d)
        pure ([], e, n)
    ETmLam (x, t) e0 -> do
        (e1, n) <- anf (introVar x 0 env) e0
        pure ([], ETmLam (x, t) e1, n+1)
    ETyLam (t, k) e0 -> do
        (e1, n) <- anf env e0
        pure ([], ETyLam (t, k) e1, n)
    ELet (Binding (x, t) e1) e2 -> do
        (bs1, e1', n) <- anf' env e1
        (x', env') <- if x `Map.member` freeVars env
            then do
                x' <- freshName
                pure (x', introRenaming x x' (introVar x' n env))
            else
                pure (x, introVar x n env)
        (bs2, e2', _) <- anf' env' e2
        pure (bs1 ++ [Binding (x', t) e1'] ++ bs2, e2', 0)
    ECase e0 as0 -> do
        anfAtomic env e0 $ \_ bs e1 _ -> do
            let fvPat p = case p of
                    CPVariant _ _ x -> Set.singleton x
                    CPEnum _ _ -> Set.empty
                    CPUnit -> Set.empty
                    CPBool _ -> Set.empty
                    CPNil -> Set.empty
                    CPCons x y -> Set.fromList [x, y]
                    CPNone -> Set.empty
                    CPSome x -> Set.singleton x
                    CPDefault -> Set.empty
            let anfAlt (CaseAlternative p e) = first (CaseAlternative p) <$> anf (introVars0 (fvPat p) env) e
            (as1, ns) <- unzip <$> traverse anfAlt as0
            pure (bs, ECase e1 as1, if null ns then 0 else minimum ns)
    ETyApp e0 t -> do
        (bs, e1, n) <- anf' env e0
        pure (bs, ETyApp e1 t, n)
    ETmApp{} -> do
        let (f0, as0) = takeETmApps e
        anfAtomic env f0 $ \_ bsf f1 n -> do
            let k = max 1 n -- we must consume at least one argument, otherwise we'll loop
            let (as1, zs1) = splitAt k as0
            (bsas, as2) <- anfMany env as1
            let (bs, e1) = (bsf ++ bsas, mkETmApps f1 as2)
            if null zs1 then
                pure (bs, e1, k - length as1)
            else do
                x <- freshName
                (bs', e2, _) <- anf' (introVar x 0 env) (mkETmApps (EVar x) zs1)
                pure (bs ++ [Binding (x, Nothing) e1] ++ bs', e2, 0)
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

anfAtomic :: AnfEnv -> Expr -> (AnfEnv -> [Binding] -> Expr -> Int -> AnfM a) -> AnfM a
anfAtomic env e0 cont = do
    (bs, e1, n) <- anf' env e0
    if isAtomic e1 then
        cont env bs e1 n
    else do
        x <- freshName
        cont (introVar x n env) (bs ++ [Binding (x, Nothing) e1]) (EVar x) n

anfMany :: Traversable t => AnfEnv -> t Expr -> AnfM ([Binding], t Expr)
anfMany env e0s = do
        let step env e0 = anfAtomic env e0 $ \env bs e1 _ -> pure (env, (bs, e1))
        (_, bse1s) <- mapAccumLM step env e0s
        pure (concatMap fst bse1s, fmap snd bse1s)

introVar :: ExprVarName -> Int -> AnfEnv -> AnfEnv
introVar x n env = env{freeVars = Map.insert x n (freeVars env)}

introVars0 :: Set ExprVarName -> AnfEnv -> AnfEnv
introVars0 xs env = env{freeVars = Map.fromSet (const 0) xs `Map.union` freeVars env}

introRenaming :: ExprVarName -> ExprVarName -> AnfEnv -> AnfEnv
introRenaming x y env = env{renamings = Map.insert x y (renamings env)}

freshName :: AnfM ExprVarName
freshName = do
    n <- state (\st -> let k = freshVarCounter st + 1 in (k, st{freshVarCounter = k}))
    pure (ExprVarName ("$v" <> T.show n))

isAtomic :: Expr -> Bool
isAtomic e = case e of
    EVar{} -> True
    EBuiltin{} -> True
    ENil{} -> True
    ENone{} -> True
    ETyApp e' _ -> isAtomic e'
    _ -> False

arity :: Expr -> Int
arity e0 = case e0 of
    ETyLam _ e1 -> arity e1
    ETmLam{} -> countTmLams e0
    ELet _ e1 -> arity e1
    ELocation _ e1 -> arity e1
    -- TODO(MH): Take partially applied top-level values into account
    _ -> 0

countTmLams :: Expr -> Int
countTmLams e0 = case e0 of
    ETmLam _ e1 -> countTmLams e1 + 1
    ETyLam _ e1 -> countTmLams e1
    ELocation _ e1 -> countTmLams e1
    _ -> 0

-- | Monadic version of mapAccumL
mapAccumLM :: forall t m acc x y. (Traversable t, Monad m) =>
    (acc -> x -> m (acc, y)) -> acc -> t x -> m (acc, t y)
mapAccumLM f z0 xs =
    let g x = StateT $ \z -> swap <$> f z x in
    swap <$> runStateT (traverse g xs) z0
