module Terms where

import Data.Fin
import Data.Nat

-- Expressions are indexed by the number of variables in scope.
-- Variables are represented by 'de Bruijn levels'. (Not *indices*!)

data Expr (vars :: Nat)
    = Var (Fin vars) 
    | App (Expr vars) (Expr vars)
    | Lam (Expr (S vars))
