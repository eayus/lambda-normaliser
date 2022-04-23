module Values where

import Terms
import Util


-- Values are the semantic meaning for an expression.
-- We also use de Bruijn levels here.

data Value (vars :: Nat)
    = VVar (Fin vars)
    | VApp (Value vars) (Value vars)
    | VLam (Closure vars)


-- Environments map one scope to another. They could be interepreted as a
-- form of explicit substitution. We use a right-extending environment,
-- but then index from the left to simulate de Bruijn levels.

type Env (from :: Nat) (to :: Nat) = RVect from (Value to)


-- A closure is a suspended computation. When evaluating lambda expressions,
-- we do not know whether the lambda will be applied yet. So we suspend the
-- evaluation in the closure.

data Closure :: Nat -> * where
    Lazily :: SNat from -> Env from vars -> Expr (S from) -> Closure vars
