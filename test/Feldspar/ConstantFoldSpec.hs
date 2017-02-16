module Feldspar.ConstantFoldSpec
  ( spec
  ) where

import Feldspar.AST (pattern (:!), pattern I32, Name(..))
import Feldspar.ConstantFold (constantFold)
import Feldspar.Intrinsic (Intrinsic(..))
import Prelude
import Test.Hspec (describe, it, Spec)
import Zabt (pattern Var)

spec :: Spec
spec = do
  describe "constantFold" $ do
    it "integer addition" $
      constantFold (Var (Intrinsic AddI32) :! I32 1 :! I32 2)
        == I32 3
    it "integer addition identity elimination on the left" $
      constantFold (Var (Intrinsic AddI32) :! Var "x" :! I32 0)
        == Var "x"
    it "integer addition identity elimination on the right" $
      constantFold (Var (Intrinsic AddI32) :! I32 0 :! Var "x")
        == Var "x"

    it "integer multiplication" $
      constantFold (Var (Intrinsic MulI32) :! I32 2 :! I32 3)
        == I32 6
    it "integer multiplication annihilation on the left" $
      constantFold (Var (Intrinsic MulI32) :! Var "x" :! I32 0)
        == I32 0
    it "integer multiplication annihilation on the right" $
      constantFold (Var (Intrinsic MulI32) :! I32 0 :! Var "x")
        == I32 0
    it "integer multiplication identity elimination on the left" $
      constantFold (Var (Intrinsic MulI32) :! Var "x" :! I32 1)
        == Var "x"
    it "integer multiplication identity elimination on the right" $
      constantFold (Var (Intrinsic MulI32) :! I32 1 :! Var "x")
        == Var "x"
