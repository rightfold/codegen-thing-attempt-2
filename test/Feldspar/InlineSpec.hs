module Feldspar.InlineSpec
  ( spec
  ) where

import Feldspar.AST (pattern (:\), pattern (:!), pattern Let)
import Feldspar.Inline (inline)
import Prelude
import Test.Hspec (describe, it, Spec)
import Zabt (pattern Var)

spec :: Spec
spec = do
  describe "inline" $ do
    it "trivial Var" $ inline (Var "x") == Var "x"
    it "trivial Let" $
      inline (Let "x" (Var "y") (Var "x"))
        == Let "x" (Var "y") (Var "y")
    it "nontrivial Let" $
      inline (Let "x" (Var "x" :! Var "y") (Var "x"))
        == Let "x" (Var "x" :! Var "y") (Var "x")
    it "trivial Lam" $
      inline (Let "x" ("a" :\ Var "b") (Var "x"))
        == Let "x" ("a" :\ Var "b") ("a" :\ Var "b")
    it "capture-avoidance" $
      inline (Let "x" (Var "y") ("y" :\ Var "x"))
        == Let "x" (Var "y") ("z" :\ Var "y")
