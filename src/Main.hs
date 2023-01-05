module Main where

import Data.Nat
import Data.Fin
import qualified Data.Vect as Vect
import Terms
import Eval

-- Small examples showcasing the evaluator...

norm' :: Expr Z -> Expr Z
norm' = norm SZ Vect.Nil


-- Render expressions in a simple way (still using de Bruijn levels)

showExpr :: Expr vars -> String
showExpr (Var v)   = show $ natToInt $ finToNat v
showExpr (App x y) = "(" ++ showExpr x ++ " " ++ showExpr y ++ ")"
showExpr (Lam x)   = "(Lam => " ++ showExpr x ++ ")"


go :: Expr Z -> IO ()
go = putStrLn . showExpr . norm'


-- Church arithemtic expressions

ezero = Lam $ Lam $ Var (FS FZ)

esucc = Lam $ Lam $ Lam $ App (Var (FS FZ)) $ App (App (Var FZ) (Var $ FS FZ)) $ Var $ FS $ FS FZ

eplus = Lam $ Lam $ Lam $ Lam $ App (App (Var FZ) (Var $ FS $ FS FZ)) $ App (App (Var $ FS FZ) (Var $ FS $ FS FZ)) $ Var $ FS $ FS $ FS FZ

etwo = App esucc $ App esucc ezero

efour = App (App eplus etwo) etwo


main :: IO ()
main = do
    putStr "zero: " >> go ezero
    putStr "succ: " >> go esucc
    putStr "plus: " >> go eplus
    putStr "two:  " >> go etwo
    putStr "four: " >> go efour
