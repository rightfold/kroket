module Frituur.Anf.Translate
  ( -- * Definitions
    translateDefinition

    -- * Terms
  , TranslateTermT
  , translateTerm
  ) where

import Control.Lens ((?~), at, view)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.HashMap.Strict (HashMap)

import qualified Control.Monad.Reader.Class as Reader
import qualified Data.HashMap.Strict as HM

import Frituur.Ast

import Frituur.Anf (AnfDefinition (..), AnfValue)
import Frituur.Name (Identifier)

import qualified Frituur.Anf.Generate as A

--------------------------------------------------------------------------------
-- Definitions

translateDefinition :: Monad m => Definition a -> m [AnfDefinition a]

translateDefinition (HazardDefinition _ _) =
  pure []

translateDefinition (ValueDefinition a name _ body) = do
  body' <- runReaderT (A.runGenerateT (translateTerm body)) HM.empty
  pure [ AnfDefinition a name body' ]

--------------------------------------------------------------------------------
-- Terms

type TranslateTermT a m =
  A.GenerateT a (ReaderT (HashMap Identifier (AnfValue a)) m)

translateTerm :: Monad m => Term a -> TranslateTermT a m (AnfValue a)

translateTerm (ApplyTerm a left right) = do
  left'  <- translateTerm left
  right' <- translateTerm right
  A.apply a left' right'

translateTerm (GetRecordFieldTerm a record field) = do
  record' <- translateTerm record
  A.getRecordField a record' field

translateTerm (IntrinsicTerm a intrinsic) =
  A.intrinsic a intrinsic

translateTerm (LambdaTerm a parameter body) =
  A.lambda a $ \argument ->
    Reader.local (at parameter ?~ argument) $
      translateTerm body

translateTerm (LiteralTerm a literal) =
  A.literal a literal

translateTerm (MakeRecordTerm a fields) = do
  fields' <- traverse (traverse translateTerm) fields
  A.makeRecord a fields'

translateTerm (RiskTerm _ _ risky) =
  translateTerm risky

translateTerm (VariableTerm a name) =
  view (at name) >>= maybe (A.global a name) pure
