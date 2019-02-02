{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Frituur.Anf
  ( -- * Names
    Temporary (..)

    -- * Nodes
  , AnfDefinition (..)
  , AnfProgram (..)
  , AnfExpression (..)
  , AnfValue (..)

    -- * Annotations
  , HasAnnotation (..)

    -- * Optics
  , anfDefinitionAnnotation
  , anfDefinitionName
  , anfDefinitionBody
  , anfBindings
  , anfResult
  , anfFreeLocals
  ) where

import Control.Category ((>>>))
import Control.Lens (Lens, (&), (.~), at, makeLenses, taking, unsafeSingular)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Sequence (Seq)
import Data.Word (Word64)

import qualified Data.HashSet as HS

import Frituur.Intrinsic (Intrinsic)
import Frituur.Literal (Literal)
import Frituur.Name (Identifier)

--------------------------------------------------------------------------------
-- Names

newtype Temporary =
  Temporary { temporaryId :: Word64 }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable)

--------------------------------------------------------------------------------
-- Nodes

data AnfDefinition a =
  AnfDefinition
    { _anfDefinitionAnnotation :: a
    , _anfDefinitionName       :: Identifier
    , _anfDefinitionBody       :: AnfProgram a }
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data AnfProgram a =
  AnfProgram
    { _anfBindings :: Seq (Temporary, AnfExpression a)
    , _anfResult   :: AnfValue a }
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data AnfExpression a
  = AnfApply a (AnfValue a) (AnfValue a)
  | AnfGetRecordField a (AnfValue a) Identifier
  | AnfLambda a Temporary (AnfProgram a)
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

data AnfValue a
  = AnfLocal a Temporary
  | AnfGlobal a Identifier
  | AnfIntrinsic a Intrinsic
  | AnfLiteral a Literal
  deriving stock (Eq, Foldable, Functor, Show, Traversable)

--------------------------------------------------------------------------------
-- Annotations

class HasAnnotation s t a b | s t -> a b where
  annotation :: Lens s t a b

instance HasAnnotation (AnfExpression a) (AnfExpression a) a a where
  annotation = unsafeSingular (taking 1 traverse)

instance HasAnnotation (AnfValue a) (AnfValue a) a a where
  annotation = unsafeSingular (taking 1 traverse)

--------------------------------------------------------------------------------
-- Optics

$(makeLenses ''AnfDefinition)
$(makeLenses ''AnfProgram)

class HasAnfFreeLocals s where
  anfFreeLocals :: s -> HashSet Temporary

instance HasAnfFreeLocals (AnfProgram a) where
  anfFreeLocals (AnfProgram bindings result) =
    let step (t, e) = HS.delete t >>> HS.union (anfFreeLocals e) in
    foldr step (anfFreeLocals result) bindings

instance HasAnfFreeLocals (AnfExpression a) where
  anfFreeLocals (AnfApply _ a b) = anfFreeLocals a <> anfFreeLocals b
  anfFreeLocals (AnfGetRecordField _ a _) = anfFreeLocals a
  anfFreeLocals (AnfLambda _ a b) = anfFreeLocals b & at a .~ Nothing

instance HasAnfFreeLocals (AnfValue a) where
  anfFreeLocals (AnfLocal _ a) = HS.singleton a
  anfFreeLocals (AnfGlobal _ _) = HS.empty
  anfFreeLocals (AnfIntrinsic _ _) = HS.empty
  anfFreeLocals (AnfLiteral _ _) = HS.empty
