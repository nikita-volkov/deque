{-|
Definitions of strict Deque.

The typical `toList` and `fromList` conversions are provided by means of
the `Foldable` and `IsList` instances.
-}
module Deque.Strict.Defs
where

import Control.Monad (fail)
import Deque.Prelude hiding (tail, init, last, head, null, dropWhile, takeWhile, reverse, filter, take)
import qualified StrictList

-- |
-- Strict double-ended queue (aka Dequeue or Deque) based on head-tail linked list.
data Deque a = Deque {-# UNPACK #-} !(StrictList.List a) {-# UNPACK #-} !(StrictList.List a)

-- |
-- /O(n)/.
-- Construct from cons and snoc lists.
fromConsAndSnocLists :: [a] -> [a] -> Deque a
fromConsAndSnocLists consList snocList = Deque (fromList consList) (fromList snocList)

-- |
-- /O(1)/.
-- Add element in the beginning.
cons :: a -> Deque a -> Deque a
cons a (Deque consList snocList) = Deque (StrictList.Cons a consList) snocList

-- |
-- /O(1)/.
-- Add element in the ending.
snoc :: a -> Deque a -> Deque a
snoc a (Deque consList snocList) = Deque consList (StrictList.Cons a snocList)

-- |
-- /O(1)/.
-- Reverse the deque.
reverse :: Deque a -> Deque a
reverse (Deque consList snocList) = Deque snocList consList

-- |
-- /O(1)/, occasionally /O(n)/.
-- Move the first element to the end.
--
-- @
-- λ toList . shiftLeft $ fromList [1,2,3]
-- [2,3,1]
-- @
shiftLeft :: Deque a -> Deque a
shiftLeft deque = maybe deque (uncurry snoc) (uncons deque)

-- |
-- /O(1)/, occasionally /O(n)/.
-- Move the last element to the beginning.
--
-- @
-- λ toList . shiftRight $ fromList [1,2,3]
-- [3,1,2]
-- @
shiftRight :: Deque a -> Deque a
shiftRight deque = maybe deque (uncurry cons) (unsnoc deque)

-- |
-- /O(n)/.
-- Leave only the elements satisfying the predicate.
filter :: (a -> Bool) -> Deque a -> Deque a
filter predicate (Deque consList snocList) = let
  newConsList = StrictList.prependReversed
    (StrictList.filterReversed predicate consList)
    (StrictList.filterReversed predicate snocList)
  in Deque newConsList StrictList.Nil

-- |
-- /O(n)/.
-- Leave only the specified amount of first elements.
take :: Int -> Deque a -> Deque a
take amount (Deque consList snocList) = let
  newSnocList = let
    buildFromConsList amount !list = if amount > 0
      then \ case
        StrictList.Cons head tail -> buildFromConsList (pred amount) (StrictList.Cons head list) tail
        _ -> buildFromSnocList amount list (StrictList.reverse snocList)
      else const list
    buildFromSnocList amount !list = if amount > 0
      then \ case
        StrictList.Cons head tail -> buildFromSnocList (pred amount) (StrictList.Cons head list) tail
        _ -> list
      else const list
    in buildFromConsList amount StrictList.Nil consList
  in Deque StrictList.Nil newSnocList

-- |
-- /O(n)/.
-- Leave only the first elements satisfying the predicate.
takeWhile :: (a -> Bool) -> Deque a -> Deque a
takeWhile predicate (Deque consList snocList) = let
  newConsList = foldr
    (\ a nextState -> if predicate a
      then StrictList.Cons a nextState
      else StrictList.Nil)
    (StrictList.takeWhileFromEnding predicate snocList)
    consList
  in Deque newConsList StrictList.Nil

-- |
-- /O(n)/.
-- Drop the first elements satisfying the predicate.
dropWhile :: (a -> Bool) -> Deque a -> Deque a
dropWhile predicate (Deque consList snocList) = let
  newConsList = StrictList.dropWhile predicate consList
  in case newConsList of
    StrictList.Nil -> Deque (StrictList.dropWhileFromEnding predicate snocList) StrictList.Nil
    _ -> Deque newConsList snocList

-- |
-- /O(1)/, occasionally /O(n)/.
-- Get the first element and deque without it if it's not empty.
uncons :: Deque a -> Maybe (a, Deque a)
uncons (Deque consList snocList) = case consList of
  StrictList.Cons head tail -> Just (head, Deque tail snocList)
  _ -> case StrictList.reverse snocList of
    StrictList.Cons head tail -> Just (head, Deque tail StrictList.Nil)
    _ -> Nothing

-- |
-- /O(1)/, occasionally /O(n)/.
-- Get the last element and deque without it if it's not empty.
unsnoc :: Deque a -> Maybe (a, Deque a)
unsnoc (Deque consList snocList) = case snocList of
  StrictList.Cons head tail -> Just (head, Deque consList tail)
  _ -> case StrictList.reverse consList of
    StrictList.Cons head tail -> Just (head, Deque StrictList.Nil tail)
    _ -> Nothing

-- |
-- /O(1)/. 
-- Check whether deque is empty.
null :: Deque a -> Bool
null = \ case
  Deque StrictList.Nil StrictList.Nil -> True
  _ -> False

-- |
-- /O(1)/, occasionally /O(n)/.
-- Get the first element if deque is not empty.
head :: Deque a -> Maybe a
head (Deque consList snocList) = case consList of
  StrictList.Cons head _ -> Just head
  _ -> StrictList.last snocList

-- |
-- /O(1)/, occasionally /O(n)/.
-- Get the last element if deque is not empty.
last :: Deque a -> Maybe a
last (Deque consList snocList) = case snocList of
  StrictList.Cons head _ -> Just head
  _ -> StrictList.last consList

-- |
-- /O(1)/, occasionally /O(n)/.
-- Keep all elements but the first one.
-- 
-- In case of empty deque returns an empty deque.
tail :: Deque a -> Deque a
tail (Deque consList snocList) = case consList of
  StrictList.Nil -> Deque (StrictList.initReversed snocList) StrictList.Nil
  _ -> Deque (StrictList.tail consList) snocList

-- |
-- /O(1)/, occasionally /O(n)/.
-- Keep all elements but the last one.
-- 
-- In case of empty deque returns an empty deque.
init :: Deque a -> Deque a
init (Deque consList snocList) = case snocList of
  StrictList.Nil -> Deque StrictList.Nil (StrictList.initReversed consList)
  _ -> Deque consList (StrictList.tail snocList)


instance Eq a => Eq (Deque a) where
  (==) a b = toList a == toList b

instance Show a => Show (Deque a) where
  show = show . toList

instance IsList (Deque a) where
  type Item (Deque a) = a
  fromList list = Deque StrictList.Nil (StrictList.fromListReversed list)
  toList (Deque consList snocList) = foldr (:) (toList (StrictList.reverse snocList)) consList

instance Semigroup (Deque a) where
  (<>) (Deque snocList1 consList1) (Deque snocList2 consList2) = let
    consList3 = consList1 <> StrictList.prependReversed snocList1 consList2
    snocList3 = snocList2
    in Deque consList3 snocList3

instance Monoid (Deque a) where
  mempty = Deque StrictList.Nil StrictList.Nil
  mappend = (<>)

deriving instance Functor Deque

instance Foldable Deque where
  foldr step init (Deque consList snocList) = foldr step (foldr step init (StrictList.reverse snocList)) consList
  foldl' step init (Deque consList snocList) = foldl' step (foldl' step init consList) (StrictList.reverse snocList)

instance Traversable Deque where
  traverse f (Deque cs ss) =
    (\cs' ss' -> Deque cs' (StrictList.reverse ss')) <$> traverse f cs <*> traverse f (StrictList.reverse ss)

instance Applicative Deque where
  pure a = Deque (pure a) StrictList.Nil
  fs <*> as = fromList (toList fs <*> toList as)

instance Monad Deque where
  return = pure
  m >>= f = fromList (toList m >>= toList . f)
  fail = const mempty

instance Alternative Deque where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Deque where
  mzero = empty
  mplus = (<|>)

instance MonadFail Deque where
  fail = const mempty
