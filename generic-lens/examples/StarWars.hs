{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Note: this file may contain spoilers
-- (although I would be really surprised if it did, I haven't seen the films)
module StarWars where

import           Data.Generics.Product
import           GHC.Generics

data Episode = NEWHOPE | EMPIRE | JEDI
  deriving (Generic, Show, Eq)

data Character = Character
   { name      :: String
   , friends   :: [Character]
   , appearsIn :: [Episode]
   } deriving (Generic, Show, Eq)

data Human = Human
  { name       :: String
  , friends    :: [Character]
  , appearsIn  :: [Episode]
  , homePlanet :: String
  } deriving (Generic, Show)

data Droid = Droid
  { friends         :: [Character]
  , appearsIn       :: [Episode]
  , name            :: String
  , primaryFunction :: String
  } deriving (Generic, Show)

luke :: Human
luke = Human
  { name           = "Luke Skywalker"
  , friends        = []
  , appearsIn      = [NEWHOPE, EMPIRE, JEDI]
  , homePlanet     = "Saturn (?)"
  }

r2d2 :: Droid
r2d2 = Droid
  { name            = "R2-D2"
  , friends         = [upcast luke]
  , appearsIn       = [NEWHOPE, EMPIRE, JEDI]
  , primaryFunction = "repair ships"
  }

c3po :: Droid
c3po = Droid
  { name            = "C3PO"
  , friends         = [upcast r2d2, upcast luke]
  , appearsIn       = [NEWHOPE, EMPIRE, JEDI]
  , primaryFunction = "protocol and human relations"
  }

getName :: HasField' "name" r a => r -> a
getName = getField @"name"

-- upcast :: Subtype a b => a -> b
characters :: [Character]
characters = [upcast r2d2, upcast luke, upcast c3po]

names :: [String]
names = map getName characters
-- => ["R2-D2","Luke Skywalker","C3PO"]
