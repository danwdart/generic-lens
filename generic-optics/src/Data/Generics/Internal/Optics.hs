{-# OPTIONS_HADDOCK hide #-}
module Data.Generics.Internal.Optics
  ( module Optics.Core
  , module Optics.Internal.Optic
  , normaliseLens
  , normalisePrism
  , normaliseIso
  ) where

import           Optics.Core
import           Optics.Internal.Optic

normaliseLens :: Lens s t a b -> Lens s t a b
normaliseLens l = withLens l lens
{-# INLINE normaliseLens #-}

normalisePrism :: Prism s t a b -> Prism s t a b
normalisePrism l = withPrism l prism
{-# INLINE normalisePrism #-}

normaliseIso :: Iso s t a b -> Iso s t a b
normaliseIso l = withIso l iso
{-# INLINE normaliseIso #-}
