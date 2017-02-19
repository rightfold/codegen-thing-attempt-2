module Feldspar.ArithmeticSpec
  ( spec
  ) where

import Feldspar.Arithmetic (arithmetic)
import Feldspar.AST (pattern (:!), pattern (:+), pattern (:*), pattern I32, Name(..), pattern Poison)
import Feldspar.Intrinsic (Intrinsic(..), ZeroDivisionPolicy(..))
import Prelude
import Test.Hspec (describe, it, Spec)
import Zabt (pattern Var)

spec :: Spec
spec = do
  describe "arithmetic" $ do
    it "integer addition" $
      arithmetic (I32 1 :+ I32 2)
        == I32 3
    it "integer addition identity elimination on the left" $
      arithmetic (Var "x" :+ I32 0)
        == Var "x"
    it "integer addition identity elimination on the right" $
      arithmetic (I32 0 :+ Var "x")
        == Var "x"

    it "integer multiplication" $
      arithmetic (I32 2 :* I32 3)
        == I32 6
    it "integer multiplication annihilation on the left" $
      arithmetic (Var "x" :* I32 0)
        == I32 0
    it "integer multiplication annihilation on the right" $
      arithmetic (I32 0 :* Var "x")
        == I32 0
    it "integer multiplication identity elimination on the left" $
      arithmetic (Var "x" :* I32 1)
        == Var "x"
    it "integer multiplication identity elimination on the right" $
      arithmetic (I32 1 :* Var "x")
        == Var "x"

    it "integer multiplication distributes over addition" $
      let mulXY = Var "x" :* Var "y"
          mulXZ = Var "x" :* Var "z"
          addYZ = Var "y" :+ Var "z"
      in arithmetic (mulXY :+ mulXZ)
           == (Var "x" :* addYZ)

    it "integer division" $
      arithmetic (Var (Intrinsic (DivI32 UndefinedZDP)) :! I32 10 :! I32 3)
        == I32 3
    it "integer division annihilation" $
      arithmetic (Var (Intrinsic (DivI32 UndefinedZDP)) :! I32 0 :! I32 3)
        == I32 0
    it "integer zero division policy ZeroZDP" $
      arithmetic (Var (Intrinsic (DivI32 ZeroZDP)) :! Var "x" :! I32 0)
        == I32 0
    it "integer zero division policy UndefinedZDP" $
      arithmetic (Var (Intrinsic (DivI32 UndefinedZDP)) :! Var "x" :! I32 0)
        == Poison
