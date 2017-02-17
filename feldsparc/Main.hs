module Main
  ( main
  ) where

import Data.Text (Text)
import Data.Text.IO as Text.IO
import Feldspar.Arithmetic (arithmetic)
import Feldspar.AST (pattern (:\), pattern (:!), pattern (:+), pattern (:*), Expr, pattern I32, pattern Let, prettyExpr)
import Feldspar.EtaReduce (etaReduce)
import Feldspar.Inline (Inline(..), inline)
import Prelude
import Zabt (pattern Var)

main :: IO ()
main = mapM_ Text.IO.putStrLn . take 20 . example $
  Let "f" ("x" :\ "y" :\ "z" :\ (Var "x" :* Var "y") :+ (Var "x" :* Var "z"))
          (Var "f" :! I32 3 :! I32 5 :! I32 7)

example :: Expr -> [Text]
example e =
  let e'   = inline (Inline 30) e
      e''  = etaReduce e'
      e''' = arithmetic e''
  in prettyExpr e : prettyExpr e' : prettyExpr e'' : example e'''
