module Main where
import STLC ( Tm(Const, Abs, App, Var), Ty(TyNat), run )

main :: IO()
main = do let x = App (Abs "x" TyNat (App (Const "S") (Var "x"))) (Const "O")
          print (run x)