module Feldspar.ConstantFoldSpec
  ( spec
  ) where

import Feldspar.AST (pattern (:!), pattern I32)
import Feldspar.ConstantFold (constantFold)
import Feldspar.Intrinsics (pattern AddI32)
import Prelude
import Test.Hspec (describe, it, Spec)
import Zabt (pattern Var)

spec :: Spec
spec = do
  describe "constantFold" $ do
    it "integer addition" $
      constantFold (Var AddI32 :! I32 1 :! I32 2)
        == I32 3
