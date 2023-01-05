module Eval where

import Data.Fin
import Data.Nat
import Data.Vect
import Terms
import Values
import Weaken


-- Expressions are evaluated in an environment to their values.

eval :: Env from to -> Expr from -> Value to
eval env (Var v)   = level v env
eval env (Lam x)   = VLam $ Lazily env x
eval env (App x y) = apply (eval env x) (eval env y)


-- Try to apply closures if possible..

apply :: Value vars -> Value vars -> Value vars
apply (VLam clos) arg = force clos arg
apply x           y   = VApp x y


-- When the value of a closure is required, we can force it by giving
-- the required argument.

force :: Closure vars -> Value vars -> Value vars
force (Lazily env x) arg = eval (Cons arg env) x


-- To complete normalisation, we must now convert the values back into
-- normal expressions.

reify :: SNat vars -> Value vars -> Expr vars
reify vars (VVar v)    = Var v
reify vars (VApp x y)  = App (reify vars x) (reify vars y)
reify vars (VLam clos) = Lam $ reify (SS vars) $ force (weakenClosure clos) (VVar $ limit vars)


-- Normalisation is just the composition of evaluation and reification.
-- Most of the time when calling this function, 'from' and 'to' will be
-- the same.

norm :: SNat to -> Env from to -> Expr from -> Expr to
norm to env x = reify to $ eval env x
