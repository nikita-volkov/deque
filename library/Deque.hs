module Deque where

import BasePrelude hiding (uncons, unsnoc, cons, snoc)


data Deque a =
  Deque ![a] ![a]

shiftRight :: Deque a -> Deque a
shiftRight (Deque snocList consList) =
  case consList of
    head : tail ->
      Deque (head : snocList) tail
    _ ->
      case reverse snocList of
        head : tail ->
          Deque (head : []) tail
        _ ->
          Deque snocList consList

shiftLeft :: Deque a -> Deque a
shiftLeft deque =
  fromMaybe deque $
  fmap (\(a, b) -> cons a b) $
  unsnoc deque

cons :: a -> Deque a -> Deque a
cons a (Deque snocList consList) =
  Deque (snocList) (a : consList)

snoc :: a -> Deque a -> Deque a
snoc a (Deque snocList consList) =
  Deque (a : snocList) (consList)

uncons :: Deque a -> Maybe (a, Deque a)
uncons deque =
  unconsWithoutBalancing deque <|>
  unconsWithoutBalancing (shiftRight deque)

unconsWithoutBalancing :: Deque a -> Maybe (a, Deque a)
unconsWithoutBalancing (Deque snocList consList) =
  case consList of
    head : tail ->
      Just (head, Deque snocList tail)
    _ ->
      Nothing

unsnoc :: Deque a -> Maybe (a, Deque a)
unsnoc (Deque snocList consList) =
  case snocList of
    head : tail ->
      Just (head, Deque tail consList)
    _ ->
      case reverse consList of
        head : tail ->
          Just (head, Deque tail [])
        _ ->
          Nothing
