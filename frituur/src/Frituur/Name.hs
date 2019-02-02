{-# LANGUAGE StrictData #-}

module Frituur.Name
  ( Identifier (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text (Text)

newtype Identifier =
  Identifier { identifierName :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, Hashable, IsString, ToJSON)
