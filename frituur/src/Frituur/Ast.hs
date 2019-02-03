{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Frituur.Ast
  ( -- * Nodes
    Definition (..)
  , Type (..)
  , Term (..)

    -- * Annotations
  , HasAnnotation (..)
  , Annotation (..)
  , annotationLocation
  , annotationDocumentation
  ) where

import Control.Lens (Lens, makeLenses, taking, unsafeSingular)
import Data.HashSet (HashSet)
import Data.Text (Text)

import Frituur.Intrinsic (Intrinsic)
import Frituur.Literal (Literal)
import Frituur.Location (Location)
import Frituur.Name (Identifier)

--------------------------------------------------------------------------------
-- Nodes

data Definition a
  = HazardDefinition a Identifier
  | ValueDefinition a Identifier (Type a) (Term a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data Type a
  = ApplyType a (Type a) (Type a)
  | ForallType a Identifier (Type a)
  | FunctionType a (HashSet Identifier)
  | VariableType a Identifier
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data Term a
  = ApplyTerm a (Term a) (Term a)
  | GetRecordFieldTerm a (Term a) Identifier
  | IntrinsicTerm a Intrinsic
  | LambdaTerm a Identifier (Term a)
  | LiteralTerm a Literal
  | MakeRecordTerm a [(Identifier, Term a)]
  | RiskTerm a (HashSet Identifier) (Term a)
  | VariableTerm a Identifier
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

--------------------------------------------------------------------------------
-- Annotations

data Annotation =
  Annotation
    { _annotationLocation      :: Location
    , _annotationDocumentation :: Text }
  deriving stock (Eq, Show)

class HasAnnotation s t a b | s t -> a b where
  annotation :: Lens s t a b

instance HasAnnotation (Definition a) (Definition a) a a where
  annotation = unsafeSingular (taking 1 traverse)

instance HasAnnotation (Type a) (Type a) a a where
  annotation = unsafeSingular (taking 1 traverse)

instance HasAnnotation (Term a) (Term a) a a where
  annotation = unsafeSingular (taking 1 traverse)

--------------------------------------------------------------------------------
-- Optics

$(makeLenses ''Annotation)
