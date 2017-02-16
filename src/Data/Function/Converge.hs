module Data.Function.Converge
  ( converge
  ) where

import Prelude

converge :: (Eq a) => (a -> a) -> a -> a
converge = until =<< ((==) =<<)
