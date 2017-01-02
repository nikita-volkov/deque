-- |
-- Module    : Deque
-- Copyright : (c) Nikita Volkov, 2016
-- License   : MIT
-- Maintainer: Nikita Volkov <nikita.y.volkov@mail.ru>
--
-- Double-ended queues (aka Dequeue or Deque) based on the head-tail linked list.

module Deque (
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

-- | A double-ended queue, which can be cycled. See `shiftLeft` and `shiftRight`.
data Deque a = Deque { _snoc :: [a], _cons :: [a] } deriving (Eq, Show, Functor)

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

instance Traversable Deque where
  traverse f (Deque ss cs) =
    (\cs' ss' -> Deque (P.reverse ss') cs') <$> traverse f cs <*> traverse f (P.reverse ss)

instance Applicative Deque where
  pure a = Deque [] [a]
  fs <*> as = fromList (toList fs <*> toList as)

instance Monad Deque where
  return = pure
  m >>= f = fromList (toList m >>= toList . f)

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
uncons (Deque ss (c:cs)) = Just (c, Deque ss cs)
uncons (P.reverse . _snoc -> (s:ss)) = Just (s, Deque [] ss)
uncons _ = Nothing

-- | /O(1)/, occasionally /O(n)/.
unsnoc :: Deque a -> Maybe (a, Deque a)
unsnoc (Deque (s:ss) cs) = Just (s, Deque ss cs)
unsnoc (P.reverse . _cons -> (c:cs)) = Just (c, Deque cs [])
unsnoc _ = Nothing

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
