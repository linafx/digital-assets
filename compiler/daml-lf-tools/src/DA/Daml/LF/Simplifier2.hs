{-# LANGUAGE OverloadedStrings #-}
module DA.Daml.LF.Simplifier2 (
    simplifyModule
) where

import DA.Daml.LF.Ast
import Data.Functor.Foldable
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Traversable (mapAccumL)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.NameMap as NM
import qualified Data.Set as Set
import qualified Data.Text.Extended as T

simplifyModule :: World -> Version -> Module -> Module
simplifyModule world version m =
    m{moduleValues = NM.map (simplifyDefValue world version) (moduleValues m)}

simplifyDefValue :: World -> Version -> DefValue -> DefValue
simplifyDefValue world version d = d{dvalBody = simplifyExpr world version (dvalBody d)}

simplifyExpr :: World -> Version -> Expr -> Expr
simplifyExpr _ _ = anf Set.empty Map.empty

anf :: Set ExprVarName -> Map ExprVarName ExprVarName -> Expr -> Expr
anf fvs ren e0 =
    let (bs, e1) = anf' fvs ren e0 in mkELets bs e1

anf' :: Set ExprVarName -> Map ExprVarName ExprVarName -> Expr -> ([Binding], Expr)
anf' fvs ren e = case e of
    EVar x -> ([], EVar (Map.findWithDefault x x ren))
    ETmLam (x, t) e' -> ([], ETmLam (x, t) (anf (Set.insert x fvs) ren e'))
    ETyLam (t, k) e' -> ([], ETyLam (t, k) (anf fvs ren e'))
    ELet (Binding (x, t) e1) e2 ->
        let (bs1, e1') = anf' fvs ren e1 in
        let (x', ren')
              | x `Set.member` fvs = let x' = freshName fvs in (x', Map.insert x x' ren)
              | otherwise = (x, ren)
        in
        let (bs2, e2') = anf' (Set.insert x' fvs) ren' e2 in
        (bs1 ++ [Binding (x', t) e1'] ++ bs2, e2')
    ECase e0 as0 ->
        let (bs1, e1) = anf' fvs ren e0 in
        let (bs2, e2)
              | isAtomic e1 = (bs1, e1)
              | otherwise =
                let x = freshName fvs in
                (bs1 ++ [Binding (x, Nothing) e1], EVar x)
        in
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
        in
        let anfAlt (CaseAlternative p e) = CaseAlternative p (anf (fvs `Set.union` fvPat p) ren e) in
        let as1 = map anfAlt as0 in
        (bs2, ECase e2 as1)
    ETyApp e0 t ->
        let (bs, e1) = anf' fvs ren e0 in
        (bs, ETyApp e1 t)
    EUpdate{} -> ([], e)
    EScenario{} -> ([], e)
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
    defaultAnf =
        let step fvs e0 =
                let (bs, e1) = anf' fvs ren e0 in
                if isAtomic e1 then
                    (fvs, (bs, e1))
                else
                    let x = freshName fvs in
                    (Set.insert x fvs, (bs ++ [Binding (x, Nothing) e1], EVar x))
        in
        let (_, bses) = mapAccumL step fvs (project e) in
        (concatMap fst bses, embed (fmap snd bses))

freshName :: Set ExprVarName -> ExprVarName
freshName fvs = fromJust (List.find (`Set.notMember` fvs) [ExprVarName ("$v" <> T.show n) | n <- [1 :: Int ..]])

isAtomic :: Expr -> Bool
isAtomic e = case e of
    EVar{} -> True
    EBuiltin{} -> True
    ENil{} -> True
    ENone{} -> True
    ETyApp e' _ -> isAtomic e'
    _ -> False
