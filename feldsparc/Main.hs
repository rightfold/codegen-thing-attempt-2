module Main
  ( main
  ) where

import Data.Monoid ((<>))
import Feldspar.Arithmetic (arithmetic)
import Feldspar.AST (pattern (:\), pattern (:!), pattern (:+), pattern (:*), Expr, pattern I32, pattern Let, prettyExpr)
import Feldspar.ECMAScript (exprToStmts)
import Feldspar.EtaReduce (etaReduce)
import Feldspar.Inline (Inline(..), inline')
import Language.ECMAScript.AST (JSProgram(..), JSStmt(..))
import Language.ECMAScript.Pretty (prettyJSProgram)
import Prelude
import Zabt (pattern Var)

import qualified Data.Text.IO as Text.IO

main :: IO ()
main = mapM_ display . take 20 . example $
  Let "f" ("x" :\ "y" :\ "z" :\ (Var "x" :* Var "y") :+ (Var "x" :* Var "z"))
          (Var "f" :! I32 3 :! I32 5 :! I32 7)
  where
  display :: Expr -> IO ()
  display e = do
    putStrLn $ show (prettyExpr e) <> ";"
    Text.IO.putStrLn $ prettyJSProgram $
      JSProgram $ exprToStmts (pure . JSExpr) e

example :: Expr -> [Expr]
example e =
  let e'   = inline' (Inline 30) e
      e''  = etaReduce e'
      e''' = arithmetic e''
  in e : e' : e'' : example e'''
