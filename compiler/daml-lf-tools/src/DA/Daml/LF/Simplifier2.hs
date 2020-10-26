{-# LANGUAGE OverloadedStrings #-}
module DA.Daml.LF.Simplifier2 (
    simplifyModule
) where

import Control.Monad.State.Strict
import DA.Daml.LF.Ast
import Data.Functor.Foldable
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Tuple (swap)
import qualified Data.Map.Strict as Map
import qualified Data.NameMap as NM
import qualified Data.Set as Set
import qualified Data.Text.Extended as T

data AnfEnv = AnfEnv
    { freeVars :: Set ExprVarName
    , renamings :: Map ExprVarName ExprVarName
    }

data AnfState = AnfState
    { freshVarCounter :: Int
    }

type AnfM = State AnfState

simplifyModule :: World -> Version -> Module -> Module
simplifyModule world version m =
    m{moduleValues = NM.map (simplifyDefValue world version) (moduleValues m)}

simplifyDefValue :: World -> Version -> DefValue -> DefValue
simplifyDefValue world version d = d{dvalBody = simplifyExpr world version (dvalBody d)}

simplifyExpr :: World -> Version -> Expr -> Expr
simplifyExpr _ _ e = runAnfM (anf emptyEnv e)

runAnfM :: AnfM a -> a
runAnfM act = evalState act AnfState
    { freshVarCounter = 0
    }

anf :: AnfEnv -> Expr -> AnfM Expr
anf env e0 = do
    (bs, e1) <- anf' env e0
    pure $ mkELets bs e1

anf' :: AnfEnv -> Expr -> AnfM ([Binding], Expr)
anf' env e = case e of
    EVar x -> pure ([], EVar (Map.findWithDefault x x (renamings env)))
    ETmLam (x, t) e' -> ([],) . ETmLam (x, t) <$> anf (introVar x env) e'
    ETyLam (t, k) e' -> ([],) . ETyLam (t, k) <$> anf env e'
    ELet (Binding (x, t) e1) e2 -> do
        (bs1, e1') <- anf' env e1
        (x', env') <- if x `Set.member` freeVars env
            then do
                x' <- freshName
                pure (x', introRenaming x x' (introVar x' env))
            else
                pure (x, introVar x env)
        (bs2, e2') <- anf' env' e2
        pure (bs1 ++ [Binding (x', t) e1'] ++ bs2, e2')
    ECase e0 as0 -> do
        (bs1, e1) <- anf' env e0
        (bs2, e2) <- if isAtomic e1
            then pure (bs1, e1)
            else do
                x <- freshName
                pure (bs1 ++ [Binding (x, Nothing) e1], EVar x)
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
        let anfAlt (CaseAlternative p e) = CaseAlternative p <$> anf (introVars (fvPat p) env) e
        as1 <- traverse anfAlt as0
        pure (bs2, ECase e2 as1)
    ETyApp e0 t -> do
        (bs, e1) <- anf' env e0
        pure (bs, ETyApp e1 t)
    EUpdate{} -> pure ([], e)
    EScenario{} -> pure ([], e)
    ELocation _ e' -> anf' env e'
    EVal{} -> defaultAnf
    EBuiltin{} -> defaultAnf
    ERecCon{} -> defaultAnf
    ERecProj{} -> defaultAnf
    ERecUpd{} -> defaultAnf
    EVariantCon{} -> defaultAnf
    EEnumCon{} -> defaultAnf
    EStructCon{} -> defaultAnf
    EStructProj{} -> defaultAnf
    EStructUpd{} -> defaultAnf
    ETmApp{} -> defaultAnf
    ENil{} -> defaultAnf
    ECons{} -> defaultAnf
    ESome{} -> defaultAnf
    ENone{} -> defaultAnf
    EToAny{} -> defaultAnf
    EFromAny{} -> defaultAnf
    ETypeRep{} -> defaultAnf
  where
    defaultAnf = do
        let step env e0 = do
                (bs, e1) <- anf' env e0
                if isAtomic e1 then
                    pure (env, (bs, e1))
                else do
                    x <- freshName
                    pure (introVar x env, (bs ++ [Binding (x, Nothing) e1], EVar x))
        (_, bses) <- mapAccumLM step env (project e)
        pure (concatMap fst bses, embed (fmap snd bses))

emptyEnv :: AnfEnv
emptyEnv = AnfEnv{freeVars = Set.empty, renamings = Map.empty}

introVar :: ExprVarName -> AnfEnv -> AnfEnv
introVar x env = env{freeVars = Set.insert x (freeVars env)}

introVars :: Set ExprVarName -> AnfEnv -> AnfEnv
introVars xs env = env{freeVars = xs `Set.union` freeVars env}

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

-- | Monadic version of mapAccumL
mapAccumLM :: forall t m acc x y. (Traversable t, Monad m) =>
    (acc -> x -> m (acc, y)) -> acc -> t x -> m (acc, t y)
mapAccumLM f z0 xs =
    let g x = StateT $ \z -> swap <$> f z x in
    swap <$> runStateT (traverse g xs) z0
