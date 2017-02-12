module Feldspar.ReduceSpec
  ( spec
  ) where

import Feldspar.AST (pattern (:\), pattern (:$))
import Feldspar.Reduce (reduce)
import Prelude
import Test.Hspec (describe, it, Spec)
import Zabt (pattern Var)

spec :: Spec
spec = do
  describe "reduce" $ do
    it "beta-reduces" $
      let e  = ("x" :\ ("y" :\ Var "x")) :$ Var "y"
          e' = "z" :\ Var "y"
      in reduce 10 e == e'
