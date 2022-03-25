{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE TypeApplications       #-}

{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Wrapped
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive an isomorphism between a newtype and its wrapped type.
--
-----------------------------------------------------------------------------

module Data.Generics.Wrapped
  ( Wrapped (..)
  , wrappedTo
  , wrappedFrom
  , _Unwrapped
  , _Wrapped
  )
where

import           Optics.Core
import           Optics.Internal.Optic

import           "generic-lens-core" Data.Generics.Internal.Wrapped (Context,
                                                                     derived)


-- | @since 1.1.0.0
_Unwrapped :: Wrapped s t a b => Iso s t a b
_Unwrapped = wrappedIso
{-# inline _Unwrapped #-}

-- | @since 1.1.0.0
_Wrapped :: Wrapped s t a b => Iso b a t s
_Wrapped = re wrappedIso
{-# inline _Wrapped #-}

-- | @since 1.1.0.0
class Wrapped s t a b | s -> a, t -> b where
  -- | @since 1.1.0.0
  wrappedIso :: Iso s t a b

-- | @since 1.1.0.0
wrappedTo :: forall s a. Wrapped s s a a => s -> a
wrappedTo = view (wrappedIso @s @s @a @a)
{-# INLINE wrappedTo #-}

-- | @since 1.1.0.0
wrappedFrom :: forall s a. Wrapped s s a a => a -> s
wrappedFrom = view (re wrappedIso)
{-# INLINE wrappedFrom #-}

instance Context s t a b => Wrapped s t a b where
  wrappedIso = Optic derived
  {-# INLINE wrappedIso #-}
