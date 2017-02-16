module Feldspar.FloatOut
  ( FloatOut(..)
  , floatOut
  ) where

import Control.Monad.Supply.Class (MonadSupply, demand)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.Text (Text)
import Feldspar.AST (bottomUpExprChildrenM, Expr, Name(..), isLocal, sizeExpr)
import Prelude
import Zabt (freeVars, pattern Var)

import qualified Data.Set as Set

data FloatOut = FloatOut
  { floatOutThreshold :: Int
  }

floatOut
  :: ( MonadSupply Text f m
     , MonadWriter [(Text, Expr)] m
     )
  => FloatOut
  -> Expr
  -> m Expr
floatOut f = bottomUpExprChildrenM go
  where
  go e | mayFloatOut e = perform e
       | otherwise     = pure e

  mayFloatOut e =
       sizeExpr e >= floatOutThreshold f
    && Set.null (Set.filter isLocal (freeVars e))

  perform e = do
    name <- demand
    tell [(name, e)]
    pure $ Var (Global name)
