module Reps040120 where

{-
Enumerated (enum) type
  Enum is a data type consisting of a set of named values.
  - Create a data that has three different Color constructors
  - Create a function that goes from your Color to a String of your color.
Basic Algebraic Data Type
  An algebraic data type (ADT) has one or more data constructors,
  and each data constructor can have zero or more arguments.
  - Create type to represent Vegetables. Celery has data arguments. Carrots
    need a color.
  - Create a function that takes a Vegetable and gives its Color.
  - Create a function that takes a Vegetable and give a String of the color.
Polymorphic Algebraic data type
  - Create a type called Keep that can story anything. Things can be kept in
    either a Shoebox or a Safe.
  - Create a function to take items out of the Keep.
  - Put a Red Carrot into a Shoebox(Keep) and write out what its type would be.
  - Create a Shoebox Carrot and write out what its type would be.
-}

data Color
  = Red
  | Blue
  | Green

colorToString :: Color -> String
colorToString color =
  case color of
    Red   -> "Red"
    Blue  -> "Blue"
    Green -> "Green"

data Vegetable
  = Celery
  | Carrot Color

getVegetableColor :: Vegetable -> Color
getVegetableColor (Carrot color) = color
getVegetableColor Celery = Green

getVegetableColorAsString :: Vegetable -> String
getVegetableColorAsString = colorToString . getVegetableColor

data Keep a
  = Shoebox a
  | Safe a

getOutOfKeep :: Keep a -> a
getOutOfKeep (Shoebox a) = a
getOutOfKeep (Safe a) = a

redCarrotShoebox :: Keep Vegetable
redCarrotShoebox = Shoebox $ Carrot Red

carrotShoebox :: Keep (Color -> Vegetable)
carrotShoebox = Shoebox Carrot

{--
Eq
  | Eq is a typeclass for types that can be compared for equality.
- Write out the Eq "Laws"
  * Note that the Haskell Report that defines the official Haskell language
    does not give an official laws for Eq. Despite this, there are customary
    laws given in the docs for Eq that represent the general expected practice
    for implementing Eq. The laws given in the answer here are those, with
    one omission (Substitutivity), for simplicities sake.
  - Reflexivity
  - Symmetry
  - Transitivity
  - Negation
- Define an enum type and provide an Eq instance for it
- Define a record type and provide an Eq instance for it
- Define another enum type and derive Eq for it
- Define another record type and derive Eq for it
--}
--

-- Reflexivity: x == x = True
-- Symmetry: x == y = y == x
-- Transitivity: x == y && y == z = x == z
-- Negation: x /= y = not (x == y)
--

data Tool
  = Axe
  | FishingRod
  | Net
  | Shovel

instance Eq Tool where
  x == y =
    case (x, y) of
      (Axe, Axe) -> True
      (FishingRod, FishingRod) -> True
      (Net, Net) -> True
      (Shovel, Shovel) -> True
      _ -> False

data Village =
  Village
    { villageName         :: String
    , villageIncorporated :: Bool
    }

instance Eq Village where
  x == y =
    (villageName x == villageName y) && (villageIncorporated x == villageIncorporated y)

data Butterfly
  = Monarch
  | Emperor
  | Tiger
  deriving (Eq)

data Island =
  Island
    { islandName    :: String
    , islandClimate :: String
    }
    deriving (Eq)
