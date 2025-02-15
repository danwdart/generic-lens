{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Typed
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive lenses of a given type in a product.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Typed
  ( -- *Lenses
    --
    -- $setup
    HasType (..)
  ) where

import           "this" Data.Generics.Internal.Optics
import           Optics.Core
import           Optics.Internal.Optic

import           "generic-lens-core" Data.Generics.Internal.Void
import qualified "generic-lens-core" Data.Generics.Product.Internal.Typed as Core

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> import GHC.Generics
-- >>> import Optics.Core
-- >>> :{
-- data Human
--   = Human
--     { name    :: String
--     , age     :: Int
--     , address :: String
--     , tall    :: Bool
--     }
--   | HumanNoTall
--     { name    :: String
--     , age     :: Int
--     , address :: String
--     }
--   deriving (Generic, Show)
-- human :: Human
-- human = Human "Tunyasz" 50 "London" False
-- :}

-- |Records that have a field with a unique type.
class HasType a s where
  -- |A lens that focuses on a field with a unique type in its parent type.
  --
  --  >>> human ^. typed @Int
  --  50
  --
  --  === /Type errors/
  --
  --  >>> human ^. typed @String
  --  ...
  --  ...
  --  ... The type Human contains multiple values of type [Char].
  --  ... The choice of value is thus ambiguous. The offending constructors are:
  --  ... Human
  --  ... HumanNoTall
  --  ...
  --
  --  >>> human ^. typed @Bool
  --  ...
  --  ...
  --  ... Not all constructors of the type Human contain a field of type Bool.
  --  ... The offending constructors are:
  --  ... HumanNoTall
  --  ...
  typed :: Lens s s a a
  typed
    = lens (getTyped @a) (flip (setTyped @a))
  {-# INLINE typed #-}

  -- |Get field at type.
  getTyped :: s -> a
  getTyped s = s ^. typed @a

  -- |Set field at type.
  setTyped :: a -> s -> s
  setTyped = set (typed @a)

  {-# MINIMAL typed | setTyped, getTyped #-}

instance Core.Context a s => HasType a s where
  typed = normaliseLens (Optic Core.derived)
  {-# INLINE typed #-}

instance {-# OVERLAPPING #-} HasType a a where
    getTyped = id
    {-# INLINE getTyped #-}

    setTyped a _ = a
    {-# INLINE setTyped #-}

-- | See Note [Uncluttering type signatures]
-- >>> :t typed
-- typed :: HasType a s => Lens s s a a
--
-- Note that this might not longer be needed given the above 'HasType a a' instance.
instance {-# OVERLAPPING #-} HasType a Void where
  typed = undefined
