module Main where
import STLC ( Tm(Const, Abs, App, Var), Ty(TyNat), run )
import Parser ( stlcParser )
import Text.Parsec ( parse )
import Text.Read ( Lexeme(String) )
import Control.Monad
import System.IO

-- test case: app (abs.x:(Nat)=>(app (const S) (var x))) (const O)

main :: IO() 
main = do 
          putStr "λλλλ>"
          hFlush stdout
          x <- getLine
          unless (x == ":q") $ do
            let parsed = parse stlcParser "" x
            case parsed of Right parsedTm -> print (run parsedTm)
                           Left warning -> print warning
            main