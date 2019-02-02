module Frituur.Intrinsic
  ( Intrinsic (..)
  , fromName
  ) where

import Data.Hashable (Hashable)
import Data.String (IsString)
import GHC.Generics (Generic)

data Intrinsic
  = Coerce#
  | Panic#
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

fromName :: (Eq a, IsString a) => a -> Maybe Intrinsic
fromName "coerce" = Just Coerce#
fromName "panic" = Just Panic#
fromName _ = Nothing
