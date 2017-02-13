module Feldspar.ASTSpec
  ( spec
  ) where

import Feldspar.AST (pattern (:\), pattern (:!), pattern Let, prettyExpr)
import Prelude
import Test.Hspec (describe, it, Spec)
import Zabt (pattern Var)

spec :: Spec
spec = do
  describe "prettyExpr" $ do
    it "Var" $ prettyExpr (Var "x") == "%x"
    it "App" $ prettyExpr (Var "x" :! Var "y") == "(%x %y)"
    it "Lam" $ prettyExpr ("x" :\ Var "y") == "(fun %x -> %y)"
    it "Let" $ prettyExpr (Let "x" (Var "a") (Var "b")) == "(let %x = %a in %b)"
