module Deque.Strict
(
  Deque,
  fromConsAndSnocLists,
  cons,
  snoc,
  reverse,
  shiftLeft,
  shiftRight,
  takeWhile,
  dropWhile,
  uncons,
  unsnoc,
  null,
  head,
  last,
  tail,
  init,
)
where

import Control.Monad (fail)
import Deque.Prelude hiding (tail, init, last, head, null, dropWhile, takeWhile, reverse)
import qualified Deque.StrictList as StrictList

-- |
-- Strict double-ended queue (aka Dequeue or Deque) based on head-tail linked list.
-- Can be cycled. See `shiftLeft` and `shiftRight`.
-- 
-- The typical `toList` and `fromList` conversions are provided by means of
-- the `Foldable` and `IsList` instances.
data Deque a = Deque {-# UNPACK #-} !(StrictList.List a) {-# UNPACK #-} !(StrictList.List a)

-- |
-- /O(1)/.
-- Construct from cons and snoc lists.
fromConsAndSnocLists :: [a] -> [a] -> Deque a
fromConsAndSnocLists consList snocList = Deque (fromList snocList) (fromList consList)

-- |
-- /O(1)/.
-- Prepend an element.
cons :: a -> Deque a -> Deque a
cons a (Deque snocList consList) = Deque snocList (StrictList.Cons a consList)

-- |
-- /O(1)/.
-- Append an element.
snoc :: a -> Deque a -> Deque a
snoc a (Deque snocList consList) = Deque (StrictList.Cons a snocList) consList

-- |
-- /O(1)/.
reverse :: Deque a -> Deque a
reverse (Deque snocList consList) = Deque consList snocList

-- |
-- /O(1)/, occasionally /O(n)/.
--
-- @
-- λ toList . shiftLeft $ fromList [1,2,3]
-- [2,3,1]
-- @
shiftLeft :: Deque a -> Deque a
shiftLeft deque = maybe deque (uncurry snoc) (uncons deque)

-- |
-- /O(1)/, occasionally /O(n)/.
--
-- @
-- λ toList . shiftRight $ fromList [1,2,3]
-- [3,1,2]
-- @
shiftRight :: Deque a -> Deque a
shiftRight deque = maybe deque (uncurry cons) (unsnoc deque)

-- |
-- /O(n)/.
-- Leave only the first elements satisfying the predicate.
takeWhile :: (a -> Bool) -> Deque a -> Deque a
takeWhile predicate (Deque snocList consList) = let
  newConsList = foldr
    (\ a nextState -> if predicate a
      then StrictList.Cons a nextState
      else StrictList.Nil)
    (StrictList.takeWhileFromEnding predicate snocList)
    consList
  in Deque StrictList.Nil newConsList

-- |
-- /O(n)/.
-- Drop the first elements satisfying the predicate.
dropWhile :: (a -> Bool) -> Deque a -> Deque a
dropWhile predicate (Deque snocList consList) = let
  newConsList = StrictList.dropWhile predicate consList
  in case newConsList of
    StrictList.Nil -> Deque StrictList.Nil (StrictList.dropWhileFromEnding predicate snocList)
    _ -> Deque snocList newConsList

-- |
-- /O(1)/, occasionally /O(n)/.
uncons :: Deque a -> Maybe (a, Deque a)
uncons (Deque snocList consList) = case consList of
  StrictList.Cons head tail -> Just (head, Deque snocList tail)
  _ -> case StrictList.reverse snocList of
    StrictList.Cons head tail -> Just (head, Deque StrictList.Nil tail)
    _ -> Nothing

-- |
-- /O(1)/, occasionally /O(n)/.
unsnoc :: Deque a -> Maybe (a, Deque a)
unsnoc (Deque snocList consList) = case snocList of
  StrictList.Cons head tail -> Just (head, Deque tail consList)
  _ -> case StrictList.reverse consList of
    StrictList.Cons head tail -> Just (head, Deque tail StrictList.Nil)
    _ -> Nothing

-- |
-- /O(1)/. 
null :: Deque a -> Bool
null = \ case
  Deque StrictList.Nil StrictList.Nil -> True
  _ -> False

-- |
-- /O(1)/, occasionally /O(n)/.
head :: Deque a -> Maybe a
head (Deque snocList consList) = case consList of
  StrictList.Cons head _ -> Just head
  _ -> StrictList.last snocList

-- |
-- /O(1)/, occasionally /O(n)/.
last :: Deque a -> Maybe a
last (Deque snocList consList) = case snocList of
  StrictList.Cons head _ -> Just head
  _ -> StrictList.last consList

-- |
-- /O(1)/, occasionally /O(n)/.
tail :: Deque a -> Deque a
tail (Deque snocList consList) = case consList of
  StrictList.Nil -> Deque StrictList.Nil (StrictList.reverseInit snocList)
  _ -> Deque snocList (StrictList.tail consList)

-- |
-- /O(1)/, occasionally /O(n)/.
init :: Deque a -> Deque a
init (Deque snocList consList) = case snocList of
  StrictList.Nil -> Deque (StrictList.reverseInit consList) StrictList.Nil
  _ -> Deque (StrictList.tail snocList) consList


instance Eq a => Eq (Deque a) where
  (==) a b = toList a == toList b

instance Show a => Show (Deque a) where
  show = showString "fromList " . show . toList

instance IsList (Deque a) where
  type Item (Deque a) = a
  fromList list = Deque (StrictList.fromReverseList list) StrictList.Nil
  toList (Deque snocList consList) = foldr (:) (StrictList.toReverseList snocList) consList

instance Semigroup (Deque a) where
  (<>) (Deque snocList1 consList1) (Deque snocList2 consList2) = let
    snocList3 = snocList2
    consList3 = consList1 <> StrictList.prependReversed snocList1 consList2
    in Deque snocList3 consList3

instance Monoid (Deque a) where
  mempty = Deque StrictList.Nil StrictList.Nil
  mappend = (<>)

deriving instance Functor Deque

instance Foldable Deque where
  foldr step init (Deque snocList consList) = foldr step (foldr step init consList) (StrictList.reverse snocList)
  foldl' step init (Deque snocList consList) = foldl' step (foldl' step init consList) (StrictList.reverse consList)

instance Traversable Deque where
  traverse f (Deque ss cs) =
    (\cs' ss' -> Deque (StrictList.reverse ss') cs') <$> traverse f cs <*> traverse f (StrictList.reverse ss)

instance Applicative Deque where
  pure a = Deque StrictList.Nil (pure a)
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
