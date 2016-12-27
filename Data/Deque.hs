{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module    : Data.Deque
-- Copyright : (c) Nikita Volkov, 2016
-- License   : MIT
-- Maintainer: Nikita Volkov <nikita.y.volkov@mail.ru>
--
-- Double-ended queues (aka Dequeue or Deque) based on the head-tail linked list.

module Data.Deque (
  -- * Double-ended Queues
  Deque
  -- * Construction
  , fromList
  , cons, snoc
  -- * Accessors
  , uncons, unsnoc
  , head, tail, init, last
  -- * Permutations
  , reverse
  , shiftRight, shiftLeft
  ) where

import           Data.Foldable
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Prelude as P
import           Prelude hiding (head, tail, init, last, reverse)

---

data Deque a = Deque [a] [a] deriving (Eq, Show, Functor)

instance Monoid (Deque a) where
  mempty = Deque [] []

  -- | /O(n)/.
  mappend (Deque s1 c1) (Deque s2 c2) = Deque s3 c1
    where s3 = s2 ++ foldl' (flip (:)) s1 c2

instance Foldable Deque where
  foldr step init (Deque snocList consList) =
    foldr step (foldl' (flip step) init snocList) consList
  foldl' step init (Deque snocList consList) =
    foldr' (flip step) (foldl' step init consList) snocList

-- | /O(1)/. `toList` is available from the `Foldable` instance.
fromList :: [a] -> Deque a
fromList = Deque []

-- | /O(1)/, occasionally /O(n)/.
--
-- @
-- λ toList . shiftLeft $ fromList [1,2,3]
-- [2,3,1]
-- @
shiftLeft :: Deque a -> Deque a
shiftLeft deque = maybe deque (uncurry snoc) $ uncons deque

-- | /O(1)/, occasionally /O(n)/.
--
-- @
-- λ toList . shiftRight $ fromList [1,2,3]
-- [3,1,2]
-- @
shiftRight :: Deque a -> Deque a
shiftRight deque = maybe deque (uncurry cons) $ unsnoc deque

-- | /O(1)/. Prepend an element.
cons :: a -> Deque a -> Deque a
cons a (Deque snocList consList) = Deque snocList (a : consList)

-- | /O(1)/. Append an element.
snoc :: a -> Deque a -> Deque a
snoc a (Deque snocList consList) = Deque (a : snocList) consList

-- | /O(1)/, occasionally /O(n)/.
uncons :: Deque a -> Maybe (a, Deque a)
uncons (Deque snocList consList) =
  case consList of
    head : tail -> Just (head, Deque snocList tail)
    _ ->
      case P.reverse snocList of
        head : tail ->
          Just (head, Deque [] tail)
        _ ->
          Nothing

-- | /O(1)/, occasionally /O(n)/.
unsnoc :: Deque a -> Maybe (a, Deque a)
unsnoc (Deque snocList consList) =
  case snocList of
    head : tail ->
      Just (head, Deque tail consList)
    _ ->
      case P.reverse consList of
        head : tail ->
          Just (head, Deque tail [])
        _ ->
          Nothing

-- | /O(1)/.
reverse :: Deque a -> Deque a
reverse (Deque snocList consList) = Deque consList snocList

-- | /O(1)/, occasionally /O(n)/.
head :: Deque a -> Maybe a
head = fmap fst . uncons

-- | /O(1)/, occasionally /O(n)/.
tail :: Deque a -> Deque a
tail = fromMaybe <$> id <*> fmap snd . uncons

-- | /O(1)/, occasionally /O(n)/.
init :: Deque a -> Deque a
init = fromMaybe <$> id <*> fmap snd . unsnoc

-- | /O(1)/, occasionally /O(n)/.
last :: Deque a -> Maybe a
last = fmap fst . unsnoc
