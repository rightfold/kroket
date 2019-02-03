{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Frituur.Anf.Generate
  ( -- * Infrastructure
    GenerateT (..)
  , runGenerate
  , runGenerateT

    -- * EDSL
  , apply
  , getRecordField
  , lambda
  , makeRecord

  , global
  , intrinsic
  , literal
  ) where

import Control.Lens ((^.), (%=), (<<.=), (<<+=), makeLenses)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Sequence (Seq, (|>))
import Data.Word (Word64)

import qualified Data.Sequence as S

import Frituur.Anf

import Frituur.Intrinsic (Intrinsic)
import Frituur.Literal (Literal)
import Frituur.Name (Identifier)

--------------------------------------------------------------------------------
-- Infrastructure

data State a =
  State
    { _stateSupply   :: Word64
    , _stateBindings :: Seq (Temporary, AnfExpression a) }

$(makeLenses ''State)

type Generate a = GenerateT a Identity

newtype GenerateT a m b =
  GenerateT (StateT (State a) m b)
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadReader r)

runGenerate :: Generate a (AnfValue a) -> AnfProgram a
runGenerate = runIdentity . runGenerateT

runGenerateT :: Monad m => GenerateT a m (AnfValue a) -> m (AnfProgram a)
runGenerateT (GenerateT a) = do
  (r, s) <- runStateT a (State 0 S.empty)
  pure $ AnfProgram (s ^. stateBindings) r

freshTemporary :: Monad m => GenerateT a m Temporary
freshTemporary = Temporary <$> GenerateT (stateSupply <<+= 1)

emitBinding :: Monad m => AnfExpression a -> GenerateT a m (AnfValue a)
emitBinding expression = do
  let a = expression ^. annotation
  result <- freshTemporary
  GenerateT $ stateBindings %= (|> (result, expression))
  pure $ AnfLocal a result

--------------------------------------------------------------------------------
-- EDSL

apply :: Monad m => a -> AnfValue a -> AnfValue a -> GenerateT a m (AnfValue a)
apply a left right = emitBinding (AnfApply a left right)

getRecordField :: Monad m => a -> AnfValue a -> Identifier -> GenerateT a m (AnfValue a)
getRecordField a record field = emitBinding (AnfGetRecordField a record field)

lambda
  :: Monad m
  => a
  -> (AnfValue a -> GenerateT a m (AnfValue a))
  -> GenerateT a m (AnfValue a)
lambda a body = do
  argument <- freshTemporary

  -- Compute the body with no bindings in the state. Afterwards, retrieve the
  -- new bindings and restore the old bindings. This allows the same supply to
  -- be used, which is important.
  oldBindings <- GenerateT $ stateBindings <<.= S.empty
  result      <- body (AnfLocal a argument)
  bindings    <- GenerateT $ stateBindings <<.= oldBindings

  emitBinding $ AnfLambda a argument (AnfProgram bindings result)

makeRecord :: Monad m => a -> [(Identifier, AnfValue a)] -> GenerateT a m (AnfValue a)
makeRecord a fields = emitBinding (AnfMakeRecord a fields)

global :: Applicative m => a -> Identifier -> m (AnfValue a)
global = (pure .) . AnfGlobal

intrinsic :: Applicative m => a -> Intrinsic -> m (AnfValue a)
intrinsic = (pure .) . AnfIntrinsic

literal :: Applicative m => a -> Literal -> m (AnfValue a)
literal = (pure .) . AnfLiteral
