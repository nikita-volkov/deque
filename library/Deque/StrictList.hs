module Deque.StrictList where

import Deque.Prelude hiding (takeWhile, dropWhile, reverse)


data List a = Cons !a !(List a) | Nil deriving
  (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance IsList (List a) where
  type Item (List a) = a
  fromList = reverse . fromReverseList
  toList = \ case
    Cons head tail -> head : toList tail
    _ -> []

instance Semigroup (List a) where
  (<>) a = prependReversed (reverse a)

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) fList aList = reverse (reverseAp fList aList)

instance Alternative List where
  empty = mempty
  (<|>) = mappend

reverseAp :: List (a -> b) -> List a -> List b
reverseAp = let
  loop bList = \ case
    Cons f fTail -> \ case
      Cons a aTail -> loop (Cons (f a) bList) fTail aTail
      _ -> bList
    _ -> const bList
  in loop Nil

filterReverse :: (a -> Bool) -> List a -> List a
filterReverse predicate = let
  loop !newList = \ case
    Cons head tail -> if predicate head
      then loop (Cons head newList) tail
      else loop newList tail
    Nil -> newList
  in loop Nil

filter :: (a -> Bool) -> List a -> List a
filter predicate = reverse . filterReverse predicate

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile predicate = \ case
  Cons head tail -> if predicate head
    then Cons head (takeWhile predicate tail)
    else Nil
  Nil -> Nil

reverse :: List a -> List a
reverse = foldl' (\ newList a -> Cons a newList) Nil

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile predicate = \ case
  Cons head tail -> if predicate head
    then dropWhile predicate tail
    else Cons head tail
  Nil -> Nil

{-|
Same as @(`takeWhile` predicate . `reverse`)@.
E.g., 

>>> takeWhileFromEnding (> 2) (fromList [1,4,2,3,4,5])
fromList [5,4,3]
-}
takeWhileFromEnding :: (a -> Bool) -> List a -> List a
takeWhileFromEnding predicate = foldl'
  (\ newList a -> if predicate a
    then Cons a newList
    else Nil)
  Nil

{-|
Same as @(`dropWhile` predicate . `reverse`)@.
E.g., 

>>> dropWhileFromEnding (> 2) (fromList [1,4,2,3,4,5])
fromList [2,4,1]
-}
dropWhileFromEnding :: (a -> Bool) -> List a -> List a
dropWhileFromEnding predicate = let
  loop confirmed unconfirmed = \ case
    Cons head tail -> if predicate head
      then loop confirmed (Cons head unconfirmed) tail
      else let
        !newConfirmed = Cons head unconfirmed
        in loop newConfirmed newConfirmed tail
    Nil -> confirmed
  in loop Nil Nil

prependReversed :: List a -> List a -> List a
prependReversed = \ case
  Cons head tail -> prependReversed tail . Cons head
  Nil -> id

head :: List a -> Maybe a
head = \ case
  Cons head _ -> Just head
  _ -> Nothing

last :: List a -> Maybe a
last = let
  loop previous = \ case
    Cons head tail -> loop (Just head) tail
    _ -> previous
  in loop Nothing

tail :: List a -> List a
tail = \ case
  Cons head tail -> tail
  Nil -> Nil

reverseInit :: List a -> List a
reverseInit = let
  loop !confirmed unconfirmed = \ case
    Cons head tail -> loop unconfirmed (Cons head unconfirmed) tail
    _ -> confirmed
  in loop Nil Nil

init :: List a -> List a
init = reverse . reverseInit

fromReverseList :: [a] -> List a
fromReverseList = let
  loop !acc = \ case
    head : tail -> loop (Cons head acc) tail
    _ -> acc
  in loop Nil

toReverseList :: List a -> [a]
toReverseList = let
  loop !list = \ case
    Cons head tail -> loop (head : list) tail
    _ -> list
  in loop []
