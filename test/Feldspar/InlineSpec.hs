module Feldspar.InlineSpec
  ( spec
  ) where

import Feldspar.AST (pattern (:\), pattern (:!), pattern I32, pattern Let)
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
        == Var "y"
    it "nontrivial Let" $
      inline (Let "x" (Var "x" :! Var "y") (Var "x"))
        == Let "x" (Var "x" :! Var "y") (Var "x")
    it "trivial Lam" $
      inline (Let "x" ("a" :\ Var "b") (Var "x"))
        == ("a" :\ Var "b")
    it "constants" $
      inline (Let "x" (I32 42) (Var "x"))
        == I32 42
    it "repeated inlining" $
      inline (Let "x" ("a" :\ Var "a") (Var "x" :! Var "y"))
        == Var "y"
    it "capture-avoidance" $
      inline (Let "x" (Var "y") ("y" :\ Var "x"))
        == ("z" :\ Var "y")
