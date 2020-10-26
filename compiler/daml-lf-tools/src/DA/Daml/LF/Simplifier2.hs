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
simplifyExpr _ _ e = runAnfM (anf Set.empty Map.empty e)

runAnfM :: AnfM a -> a
runAnfM act = evalState act AnfState
    { freshVarCounter = 0
    }

anf :: Set ExprVarName -> Map ExprVarName ExprVarName -> Expr -> AnfM Expr
anf fvs ren e0 = do
    (bs, e1) <- anf' fvs ren e0
    pure $ mkELets bs e1

anf' :: Set ExprVarName -> Map ExprVarName ExprVarName -> Expr -> AnfM ([Binding], Expr)
anf' fvs ren e = case e of
    EVar x -> pure ([], EVar (Map.findWithDefault x x ren))
    ETmLam (x, t) e' -> ([],) . ETmLam (x, t) <$> anf (Set.insert x fvs) ren e'
    ETyLam (t, k) e' -> ([],) . ETyLam (t, k) <$> anf fvs ren e'
    ELet (Binding (x, t) e1) e2 -> do
        (bs1, e1') <- anf' fvs ren e1
        (x', ren') <- if x `Set.member` fvs
            then do
                x' <- freshName
                pure (x', Map.insert x x' ren)
            else
                pure (x, ren)
        (bs2, e2') <- anf' (Set.insert x' fvs) ren' e2
        pure (bs1 ++ [Binding (x', t) e1'] ++ bs2, e2')
    ECase e0 as0 -> do
        (bs1, e1) <- anf' fvs ren e0
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
        let anfAlt (CaseAlternative p e) = CaseAlternative p <$> anf (fvs `Set.union` fvPat p) ren e
        as1 <- traverse anfAlt as0
        pure (bs2, ECase e2 as1)
    ETyApp e0 t -> do
        (bs, e1) <- anf' fvs ren e0
        pure (bs, ETyApp e1 t)
    EUpdate{} -> pure ([], e)
    EScenario{} -> pure ([], e)
    ELocation _ e' -> anf' fvs ren e'
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
        let step fvs e0 = do
                (bs, e1) <- anf' fvs ren e0
                if isAtomic e1 then
                    pure (fvs, (bs, e1))
                else do
                    x <- freshName
                    pure (Set.insert x fvs, (bs ++ [Binding (x, Nothing) e1], EVar x))
        (_, bses) <- mapAccumLM step fvs (project e)
        pure (concatMap fst bses, embed (fmap snd bses))

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
