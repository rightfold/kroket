{-# LANGUAGE TemplateHaskell #-}

module Frituur.Interface
  ( -- * Interfaces
    Interface (..)
  , interfaceHazards
  , interfaceValues

    -- * Extraction
  , extractInterface
  ) where

import Control.Lens ((?~), at, makeLenses)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Frituur.Ast (Definition (..), Type)
import Frituur.Name (Identifier)

--------------------------------------------------------------------------------
-- Interfaces

-- |
-- The interface is the set of all top-level definitions, each annotated with
-- the information needed for type checking.
data Interface a =
  Interface
    { _interfaceHazards :: HashSet Identifier
    , _interfaceValues :: HashMap Identifier (Type a) }
  deriving stock (Eq, Show)

$(makeLenses ''Interface)

--------------------------------------------------------------------------------
-- Extraction

extractInterface :: [Definition a] -> Interface a
extractInterface definitions =
  let
    step :: Definition a -> Interface a -> Interface a

    step (HazardDefinition _ name) =
      -- TODO: Fail if a hazard with this name is already present.
      interfaceHazards . at name ?~ ()

    step (ValueDefinition _ name type_ _) =
      -- TODO: Fail if a value with this name is already present.
      interfaceValues . at name ?~ type_

  in
    foldr step (Interface HS.empty HM.empty) definitions
