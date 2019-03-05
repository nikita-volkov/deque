{-|
Definitions of lazy Deque.

The typical `toList` and `fromList` conversions are provided by means of
the `Foldable` and `IsList` instances.
-}
module Deque.Lazy.Defs
where

import Control.Monad (fail)
import Deque.Prelude hiding (tail, init, last, head, null, dropWhile, takeWhile, reverse, filter, take)
import qualified Data.List as List
import qualified Deque.Prelude as Prelude

-- |
-- Lazy double-ended queue (aka Dequeue or Deque) based on head-tail linked list.
data Deque a = Deque ![a] ![a]

-- |
-- /O(1)/.
-- Construct from cons and snoc lists.
fromConsAndSnocLists :: [a] -> [a] -> Deque a
fromConsAndSnocLists consList snocList = Deque consList snocList

-- |
-- /O(n)/.
-- Leave only the elements satisfying the predicate.
filter :: (a -> Bool) -> Deque a -> Deque a
filter predicate (Deque consList snocList) = Deque (List.filter predicate consList) (List.filter predicate snocList)

-- |
-- /O(n)/.
-- Leave only the specified amount of first elements.
take :: Int -> Deque a -> Deque a
take amount (Deque consList snocList) = let
  newConsList = let
    buildFromConsList amount = if amount > 0
      then \ case
        head : tail -> head : buildFromConsList (pred amount) tail
        _ -> buildFromSnocList amount (List.reverse snocList)
      else const []
    buildFromSnocList amount = if amount > 0
      then \ case
        head : tail -> head : buildFromSnocList (pred amount) tail
        _ -> []
      else const []
    in buildFromConsList amount consList
  in Deque newConsList []

-- |
-- /O(n)/.
-- Drop the specified amount of first elements.
drop :: Int -> Deque a -> Deque a
drop amount (Deque consList snocList) = let
  buildFromConsList amount = if amount > 0
    then \ case
      _ : tail -> buildFromConsList (pred amount) tail
      _ -> buildFromSnocList amount (List.reverse snocList)
    else \ tail -> Deque tail snocList
  buildFromSnocList amount = if amount > 0
    then \ case
      _ : tail -> buildFromSnocList (pred amount) tail
      _ -> Deque [] []
    else \ tail -> Deque tail []
  in buildFromConsList amount consList

-- |
-- /O(n)/.
-- Leave only the first elements satisfying the predicate.
takeWhile :: (a -> Bool) -> Deque a -> Deque a
takeWhile predicate (Deque consList snocList) = let
  newConsList = List.foldr
    (\ a nextState -> if predicate a
      then a : nextState
      else [])
    (List.takeWhile predicate (List.reverse snocList))
    consList
  in Deque newConsList []

-- |
-- /O(n)/.
-- Drop the first elements satisfying the predicate.
dropWhile :: (a -> Bool) -> Deque a -> Deque a
dropWhile predicate (Deque consList snocList) = let
  newConsList = List.dropWhile predicate consList
  in case newConsList of
    [] -> Deque (List.dropWhile predicate (List.reverse snocList)) []
    _ -> Deque newConsList snocList

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
-- /O(1)/.
-- Add element in the beginning.
cons :: a -> Deque a -> Deque a
cons a (Deque consList snocList) = Deque (a : consList) snocList

-- |
-- /O(1)/.
-- Add element in the ending.
snoc :: a -> Deque a -> Deque a
snoc a (Deque consList snocList) = Deque consList (a : snocList)

-- |
-- /O(1)/, occasionally /O(n)/.
-- Get the first element and deque without it if it's not empty.
uncons :: Deque a -> Maybe (a, Deque a)
uncons (Deque consList snocList) = case consList of
  head : tail -> Just (head, Deque tail snocList)
  _ -> case List.reverse snocList of
    head : tail -> Just (head, Deque tail [])
    _ -> Nothing

-- |
-- /O(1)/, occasionally /O(n)/.
-- Get the last element and deque without it if it's not empty.
unsnoc :: Deque a -> Maybe (a, Deque a)
unsnoc (Deque consList snocList) = case snocList of
  head : tail -> Just (head, Deque consList tail)
  _ -> case List.reverse consList of
    head : tail -> Just (head, Deque [] tail)
    _ -> Nothing

-- |
-- /O(n)/.
prepend :: Deque a -> Deque a -> Deque a
prepend (Deque consList1 snocList1) (Deque consList2 snocList2) = let
  consList = consList1
  snocList = snocList2 ++ foldl' (flip (:)) snocList1 consList2
  in Deque consList snocList

-- |
-- /O(1)/.
-- Reverse the deque.
reverse :: Deque a -> Deque a
reverse (Deque consList snocList) = Deque snocList consList

-- |
-- /O(1)/. 
-- Check whether deque is empty.
null :: Deque a -> Bool
null (Deque consList snocList) = List.null snocList && List.null consList

-- |
-- /O(1)/, occasionally /O(n)/.
-- Get the first element if deque is not empty.
head :: Deque a -> Maybe a
head = fmap fst . uncons

-- |
-- /O(1)/, occasionally /O(n)/.
-- Keep all elements but the first one.
-- 
-- In case of empty deque returns an empty deque.
tail :: Deque a -> Deque a
tail = fromMaybe <$> id <*> fmap snd . uncons

-- |
-- /O(1)/, occasionally /O(n)/.
-- Keep all elements but the last one.
-- 
-- In case of empty deque returns an empty deque.
init :: Deque a -> Deque a
init = fromMaybe <$> id <*> fmap snd . unsnoc

-- |
-- /O(1)/, occasionally /O(n)/.
-- Get the last element if deque is not empty.
last :: Deque a -> Maybe a
last = fmap fst . unsnoc


instance Eq a => Eq (Deque a) where
  (==) a b = toList a == toList b

instance Show a => Show (Deque a) where
  show = show . toList

instance Semigroup (Deque a) where
  (<>) = prepend

instance Monoid (Deque a) where
  mempty =
    Deque [] []
  mappend =
    (<>)

instance Foldable Deque where
  foldr step init (Deque consList snocList) = foldr step (foldl' (flip step) init snocList) consList
  foldl' step init (Deque consList snocList) = foldr' (flip step) (foldl' step init consList) snocList

instance Traversable Deque where
  traverse f (Deque cs ss) =
    (\cs' ss' -> Deque cs' (List.reverse ss')) <$> traverse f cs <*> traverse f (List.reverse ss)

deriving instance Functor Deque

instance Applicative Deque where
  pure a = Deque [] [a]
  (<*>) (Deque fnConsList fnSnocList) (Deque argConsList argSnocList) = let
    consList = let
      fnStep fn resultConsList = let
        argStep arg = (:) (fn arg)
        in foldr argStep (foldr argStep resultConsList (List.reverse argSnocList)) argConsList
      in foldr fnStep (foldr fnStep [] (List.reverse fnSnocList)) fnConsList 
    in Deque consList []

instance Monad Deque where
  return = pure
  (>>=) (Deque aConsList aSnocList) k = let
    consList = let
      aStep a accBConsList = case k a of
        Deque bConsList bSnocList -> bConsList <> List.reverse bSnocList <> accBConsList
      in foldr aStep (foldr aStep [] (List.reverse aSnocList)) aConsList
    in Deque consList []
  fail = const mempty

instance Alternative Deque where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Deque where
  mzero = empty
  mplus = (<|>)

instance MonadFail Deque where
  fail = const mempty

-- |
-- /O(1)/.
instance IsList (Deque a) where
  type Item (Deque a) = a
  fromList = flip Deque []
  toList (Deque consList snocList) = consList <> List.reverse snocList
  