module Frituur.Literal
  ( Literal (..)
  ) where

import qualified Data.Text as T

data Literal
  = Utf8Literal T.Text
  deriving stock (Eq, Ord, Show)
