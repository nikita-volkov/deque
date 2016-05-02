module Deque where

import BasePrelude hiding (uncons, unsnoc, cons, snoc, reverse)
import qualified BasePrelude


data Deque a =
  Deque ![a] ![a]

-- |
-- /O(1)/, sometimes /O(n)/.
shiftRight :: Deque a -> Deque a
shiftRight deque =
  fromMaybe deque $
  fmap (\(a, b) -> snoc a b) $
  uncons deque

-- |
-- /O(1)/, sometimes /O(n)/.
shiftLeft :: Deque a -> Deque a
shiftLeft deque =
  fromMaybe deque $
  fmap (\(a, b) -> cons a b) $
  unsnoc deque

-- |
-- /O(1)/.
cons :: a -> Deque a -> Deque a
cons a (Deque snocList consList) =
  Deque (snocList) (a : consList)

-- |
-- /O(1)/.
snoc :: a -> Deque a -> Deque a
snoc a (Deque snocList consList) =
  Deque (a : snocList) (consList)

-- |
-- /O(1)/, sometimes /O(n)/.
uncons :: Deque a -> Maybe (a, Deque a)
uncons (Deque snocList consList) =
  case consList of
    head : tail ->
      Just (head, Deque snocList tail)
    _ ->
      case BasePrelude.reverse snocList of
        head : tail ->
          Just (head, Deque [] tail)
        _ ->
          Nothing

-- |
-- /O(1)/, sometimes /O(n)/.
unsnoc :: Deque a -> Maybe (a, Deque a)
unsnoc (Deque snocList consList) =
  case snocList of
    head : tail ->
      Just (head, Deque tail consList)
    _ ->
      case BasePrelude.reverse consList of
        head : tail ->
          Just (head, Deque tail [])
        _ ->
          Nothing

-- |
-- /O(n)/.
prepend :: Deque a -> Deque a -> Deque a
prepend (Deque snocList1 consList1) (Deque snocList2 consList2) =
  Deque snocList3 consList3
  where
    snocList3 =
      snocList2 ++ BasePrelude.reverse consList2 ++ snocList1
    consList3 =
      consList1

-- |
-- /O(1)/.
reverse :: Deque a -> Deque a
reverse (Deque snocList consList) =
  Deque consList snocList

-- |
-- /O(1)/, sometimes /O(n)/.
tail :: Deque a -> Deque a
tail =
  fromMaybe <$> id <*> fmap snd . uncons

-- |
-- /O(1)/, sometimes /O(n)/.
init :: Deque a -> Deque a
init =
  fromMaybe <$> id <*> fmap snd . unsnoc


instance Monoid (Deque a) where
  mempty =
    Deque [] []
  mappend =
    prepend

instance Foldable Deque where
  foldr step init (Deque snocList consList) =
    foldr step (foldl' (flip step) init snocList) consList
  foldl' step init (Deque snocList consList) =
    foldr' (flip step) (foldl' step init consList) snocList

deriving instance Functor Deque
