module Cursor (Cursor, makeCursor, next, previous, mapWith, selected) where

data Cursor a = Cursor {left :: [a], right :: [a]}

makeCursor :: [a] -> Cursor a
makeCursor vs = Cursor {left = [], right = vs}

next :: Cursor a -> Cursor a
next c = case (left c, right c) of
  (_, [_]) -> c
  (_, []) -> c
  (l, current : r) -> Cursor {left = current : l, right = r}

previous :: Cursor a -> Cursor a
previous c = case (left c, right c) of
  ([], _) -> c
  (current : l, r) -> Cursor {left = l, right = current : r}

selected :: Cursor a -> Maybe a
selected Cursor {right = r : _} = Just r
selected _ = Nothing

mapWith :: (Bool -> a -> b) -> Cursor a -> [b]
mapWith f Cursor {left = l, right = []} =
  map (f False) . reverse $ l
mapWith f Cursor {left = l, right = current : r} =
  (map (f False) . reverse $ l) ++ (f True current : map (f False) r)
