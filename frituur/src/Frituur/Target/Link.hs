module Frituur.Target.Link
  ( LinkError (..)
  , linkObjects
  ) where

import Control.Exception (Exception)
import Control.Lens (view)

import Frituur.Target.Object (Object, objectCode)

data LinkError =
  LinkError
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- |
-- Link objects to form an executable, validating that all names that are used
-- are also defined, and that no name is defined twice.
linkObjects :: ([a] -> b) -> [Object a] -> Either LinkError b
linkObjects linkCodes objects =
  -- TODO: Check that no two objects define the same values.
  -- TODO: Check that objects do not reference undefined values.
  Right $ linkCodes (fmap (view objectCode) objects)
