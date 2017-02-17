module Feldspar.EtaReduceSpec
  ( spec
  ) where

import Feldspar.AST (pattern (:\), pattern (:!))
import Feldspar.EtaReduce (etaReduce)
import Prelude
import Test.Hspec (describe, it, Spec)
import Zabt (pattern Var)

spec :: Spec
spec = do
  describe "etaReduce" $ do
    it "eta reduce" $
      etaReduce ("x" :\ (Var "f" :! Var "x"))
        == Var "f"
    it "no eta reduce" $
      etaReduce ("x" :\ (Var "f" :! Var "y"))
        == ("x" :\ (Var "f" :! Var "y"))
    it "no eta reduce" $
      etaReduce ("x" :\ (Var "x" :! Var "x"))
        == ("x" :\ (Var "x" :! Var "x"))
