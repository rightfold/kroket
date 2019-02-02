{-# LANGUAGE TemplateHaskell #-}

module Frituur.Target.Object
  ( Object (..)
  , makeObject
  , objectValues
  , objectCode
  ) where

import Control.Lens ((^..), folded, makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashSet (HashSet)
import GHC.Generics (Generic)

import qualified Data.HashSet as HS

import Frituur.Anf (AnfDefinition, anfDefinitionName)
import Frituur.Name (Identifier)

-- |
-- An object is the output of translating one translation unit. It contains all
-- the information necessary for linking.
data Object a =
  Object
    { _objectValues :: HashSet Identifier
      -- TODO: Record also used values, not just defined values.
    , _objectCode   :: a }
  deriving stock (Eq, Foldable, Functor, Generic, Traversable, Show)
  deriving anyclass (FromJSON, ToJSON)

$(makeLenses ''Object)

-- |
-- Create an object from a list of definitions using a function that generates
-- object code from those definitions.
makeObject :: ([AnfDefinition a] -> b) -> [AnfDefinition a] -> Object b
makeObject translate ds =
  Object
    { _objectValues = HS.fromList $ ds ^.. folded . anfDefinitionName
    , _objectCode   = translate ds }
