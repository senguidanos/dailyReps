module Reps040420 where
{-
- Write a function fmapList that does what fmap does for a List
- Write a function fmapMaybe that does what fmap does for a Maybe
- Write a function foldLList function that does what foldl does for a List
-}

fmapList :: (a -> b) -> [a] -> [b]
fmapList fn list =
  case list of
    [] -> []
    a : rest -> fn a : fmapList fn rest

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe fn mbA =
  case mbA of
    Just a  -> Just $ fn a
    Nothing -> Nothing

foldLList :: (b -> a -> b) -> b -> [a] -> b
foldLList fn b list =
  case list of
    [] -> b
    a : rest -> foldLList fn (fn b a) rest

-- do it again
foldLList' :: (b -> a -> b) -> b -> [a] -> b
foldLList' fn b list =
  case list of
    [] -> b
    a:rest -> foldLList fn (fn b a) rest
