module Feldspar.ReduceSpec
  ( spec
  ) where

import Feldspar.AST (pattern (:\), pattern (:!), pattern Add', pattern Int', ExprF(..))
import Feldspar.Reduce (reduce)
import Prelude
import Test.Hspec (describe, it, Spec)
import Zabt (pattern Var)

spec :: Spec
spec = do
  describe "reduce" $ do
    it "beta-reduces" $
      let e  = ("x" :\ ("y" :\ Var "x")) :! Var "y"
          e' = "z" :\ Var "y"
      in reduce 10 e == e'

    it "adds integer constants" $
      let e  = Add' :! Int' 1 :! Int' 2
          e' = Int' 3
      in reduce 10 e == e'

    it "eliminates the additive identity on the left" $
      let e  = Add' :! Int' 0 :! Var "x"
          e' = Var "x"
      in reduce 10 e == e'

    it "eliminates the additive identity on the right" $
      let e  = Add' :! Var "x" :! Int' 0
          e' = Var "x"
      in reduce 10 e == e'
