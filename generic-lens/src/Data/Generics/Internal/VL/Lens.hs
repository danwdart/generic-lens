{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# LANGUAGE TypeFamilyDependencies    #-}

{-# LANGUAGE TupleSections             #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.VL.Lens
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal lens helpers. Only exported for Haddock
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.VL.Lens where

import           "generic-lens-core" Data.Generics.Internal.Profunctor.Lens (ALens (..),
                                                                             idLens)

import           Control.Applicative                                        (Const (..))
import           Data.Coerce                                                (coerce)
import           Data.Functor.Identity                                      (Identity (..))

-- | Type alias for lens
type Lens' s a
  = Lens s s a a

type Lens s t a b
  = forall f. Functor f => (a -> f b) -> s -> f t

view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view l s = (^.) s l

-- | Getting
(^.) :: s -> ((a -> Const a a) -> s -> Const a s) -> a
s ^. l = getConst (l Const s)
infixl 8 ^.

infixr 4 .~
(.~) :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
(.~) f b = runIdentity . f (Identity . const b)

set :: Lens s t a b -> b -> s -> t
set l x = l .~ x

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over = coerce

lens2lensvl :: ALens a b i s t -> Lens s t a b
lens2lensvl (ALens _get _set) =
  \f x ->
    case _get x of
      (c, a) -> _set . (c, ) <$> f a
{-# INLINE lens2lensvl #-}

ravel :: (ALens a b i a b -> ALens a b i s t)
      ->  Lens s t a b
ravel l = lens2lensvl $ l idLens


lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get _set f x = _set x <$> f (get x)
{-# INLINE lens #-}

