{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Frituur.Location
  ( Location (..)
  , locationLine
  , locationColumn
  ) where

import Control.Lens (makeLenses)

data Location =
  Location
    { _locationLine   :: {-# UNPACK #-} Int
    , _locationColumn :: {-# UNPACK #-} Int }
  deriving stock (Eq, Ord, Show)

$(makeLenses ''Location)
