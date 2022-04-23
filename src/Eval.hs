module Eval where

import Util
import Terms
import Values
import Weaken


-- Expressions are evaluated in an environment to their values.

eval :: SNat from -> Env from to -> Expr from -> Value to
eval from env (Var v)   = lindex from v env
eval from env (Lam x)   = VLam $ Lazily from env x
eval from env (App x y) = apply (eval from env x) (eval from env y)


-- Try to apply closures if possible..

apply :: Value vars -> Value vars -> Value vars
apply (VLam clos) arg = force clos arg
apply x           y   = VApp x y


-- When the value of a closure is required, we can force it by giving
-- the required argument.

force :: Closure vars -> Value vars -> Value vars
force (Lazily from env x) arg = eval (SS from) (Ext env arg) x


-- To complete normalisation, we must now convert the values back into
-- normal expressions.

reify :: SNat vars -> Value vars -> Expr vars
reify vars (VVar v)    = Var v
reify vars (VApp x y)  = App (reify vars x) (reify vars y)
reify vars (VLam clos) = Lam $ reify (SS vars) $ force (weakenClosure clos) (VVar $ largest vars)


-- Normalisation is just the composition of evaluation and reification.
-- Most of the time when calling this function, 'from' and 'to' will be
-- the same.

norm :: SNat from -> SNat to -> Env from to -> Expr from -> Expr to
norm from to env x = reify to $ eval from env x
