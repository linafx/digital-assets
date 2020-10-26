module DA.Daml.LF.Simplifier2 (
    simplifyModule
) where

import DA.Daml.LF.Ast
import qualified Data.NameMap as NM

simplifyModule :: World -> Version -> Module -> Module
simplifyModule world version m =
    m{moduleValues = NM.map (simplifyDefValue world version) (moduleValues m)}

simplifyDefValue :: World -> Version -> DefValue -> DefValue
simplifyDefValue world version d = d{dvalBody = simplifyExpr world version (dvalBody d)}

simplifyExpr :: World -> Version -> Expr -> Expr
simplifyExpr _ _ e = e
