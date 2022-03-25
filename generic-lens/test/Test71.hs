{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Test71 where

import           Control.Lens
import           Data.Generics.Product
import           GHC.Generics

data Foobar = Foobar Int Char String
    deriving Generic

foo :: HasType Foobar ctx => ctx -> Char
foo ctx = case view (typed @Foobar) ctx of
    Foobar _ c _ -> c

bar :: Char
bar = foo (Foobar 3 'a' "Hello")
