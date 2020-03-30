module Reps033020 where

{-
Basics of Haskell Records
- Create a record with two fields of different types
- write a function that will create an instance of your record using record syntax
- write down the type of the data constructor for your record
- write a function that will create an instance of your record using regular data constructor syntax
- write a function that will update one of the fields of an existing record with a new value
-}

data Record =
  Record
    { recordString :: String
    , recordBool   :: Bool
    }


mkRecord :: String
         -> Bool
         -> Record
mkRecord str bool =
  Record
    { recordString = str
    , recordBool   = bool
    }

recordConstructor :: String
                  -> Bool
                  -> Record
recordConstructor = Record

mkRecordWithDataSyntax :: String
                       -> Bool
                       -> Record
mkRecordWithDataSyntax str bool =
  Record str bool

updateRecordString :: String
                   -> Record
                   -> Record
updateRecordString newStr record =
  record { recordString = newStr }
