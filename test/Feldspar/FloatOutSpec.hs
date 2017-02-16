module Feldspar.FloatOutSpec
  ( spec
  ) where

import Control.Monad.Supply (runSupply)
import Control.Monad.Writer (runWriterT)
import Data.Monoid ((<>))
import Feldspar.AST (pattern (:\), pattern (:!), Name(..))
import Feldspar.FloatOut (FloatOut(..), floatOut)
import Prelude
import Test.Hspec (describe, it, Spec)
import Zabt (pattern Var)

spec :: Spec
spec = do
  describe "floatOut" $ do
    let f = FloatOut 3
    let run a = runSupply (runWriterT a) (<> "X") "X"
    it "trivial Var, free" $
      run (floatOut f (Var "x"))
        == (Var "x", [])
    it "trivial Lam, no free" $
      run (floatOut f ("x" :\ Var "x"))
        == ("x" :\ Var "x", [])
    it "nontrivial Lam, no free" $
      let e  = "f" :\ (Var "f" :! e')
          e' = ("g" :\ "x" :\ "y" :\ (Var "g" :! Var "x" :! Var "y"))
      in run (floatOut f e)
           == ( "f" :\ (Var "f" :! Var (Global "X"))
              , [("X", e')]
              )
