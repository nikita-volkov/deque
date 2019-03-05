{-|
Lazy Deque API lifted to a Reader monad, \"mtl\"-style.
-}
module Deque.Lazy.Reader
where

import Deque.Prelude hiding (tail, init, last, head, null, dropWhile, takeWhile, reverse)
import Deque.Lazy (Deque)
import qualified Deque.Lazy as Deque
import qualified Deque.Prelude as Prelude


{-|
/O(n)/.
Modify each element of the queue.
-}
map :: MonadReader (Deque a) m => (a -> b) -> m (Deque b)
map f = reader (fmap f)

{-|
/O(n)/.
Add elements to the begginning.
-}
prepend :: MonadReader (Deque a) m => Deque a -> m (Deque a)
prepend deque = reader (deque <>)

{-|
/O(n)/.
Add elements to the ending.
-}
append :: MonadReader (Deque a) m => Deque a -> m (Deque a)
append deque = reader (<> deque)

{-|
/O(1)/.
Add element in the beginning.
-}
cons :: MonadReader (Deque a) m => a -> m (Deque a)
cons a = reader (Deque.cons a)

{-|
/O(1)/.
Add element in the ending.
-}
snoc :: MonadReader (Deque a) m => a -> m (Deque a)
snoc a = reader (Deque.snoc a)

{-|
/O(1)/.
Reverse the deque.
-}
reverse :: MonadReader (Deque a) m => m (Deque a)
reverse = reader Deque.reverse

{-|
/O(1)/, occasionally /O(n)/.
Move the first element to the end.
-}
shiftLeft :: MonadReader (Deque a) m => m (Deque a)
shiftLeft = reader Deque.shiftLeft

{-|
/O(1)/, occasionally /O(n)/.
Move the last element to the beginning.
-}
shiftRight :: MonadReader (Deque a) m => m (Deque a)
shiftRight = reader Deque.shiftRight

{-|
/O(n)/.
Leave only the elements satisfying the predicate.
-}
filter :: MonadReader (Deque a) m => (a -> Bool) -> m (Deque a)
filter predicate = reader (Deque.filter predicate)

{-|
/O(n)/.
Leave only the specified amount of first elements.
-}
take :: MonadReader (Deque a) m => Int -> m (Deque a)
take = reader . Deque.take

{-|
/O(n)/.
Drop the specified amount of first elements.
-}
drop :: MonadReader (Deque a) m => Int -> m (Deque a)
drop = reader . Deque.drop

{-|
/O(n)/.
Leave only the first elements satisfying the predicate.
-}
takeWhile :: MonadReader (Deque a) m => (a -> Bool) -> m (Deque a)
takeWhile predicate = reader (Deque.takeWhile predicate)

{-|
/O(n)/.
Drop the first elements satisfying the predicate.
-}
dropWhile :: MonadReader (Deque a) m => (a -> Bool) -> m (Deque a)
dropWhile predicate = reader (Deque.dropWhile predicate)

{-|
/O(1)/, occasionally /O(n)/.
Get the first element and deque without it if it's not empty.
-}
uncons :: MonadReader (Deque a) m => m (Maybe a, Deque a)
uncons = reader (\ deque -> case Deque.uncons deque of
  Nothing -> (Nothing, deque)
  Just (a, newDeque) -> (Just a, newDeque))

{-|
/O(1)/, occasionally /O(n)/.
Get the last element and deque without it if it's not empty.
-}
unsnoc :: MonadReader (Deque a) m => m (Maybe a, Deque a)
unsnoc = reader (\ deque -> case Deque.unsnoc deque of
  Nothing -> (Nothing, deque)
  Just (a, newDeque) -> (Just a, newDeque))

{-|
/O(1)/. 
Check whether deque is empty.
-}
null :: MonadReader (Deque a) m => m Bool
null = reader Deque.null

{-|
/O(1)/. 
Check whether deque is empty.
-}
length :: MonadReader (Deque a) m => m Int
length = reader Prelude.length

{-|
/O(1)/, occasionally /O(n)/.
Get the first element if deque is not empty.
-}
head :: MonadReader (Deque a) m => m (Maybe a)
head = reader Deque.head

{-|
/O(1)/, occasionally /O(n)/.
Get the last element if deque is not empty.
-}
last :: MonadReader (Deque a) m => m (Maybe a)
last = reader Deque.last

{-|
/O(1)/, occasionally /O(n)/.
Keep all elements but the first one.
-}
tail :: MonadReader (Deque a) m => m (Deque a)
tail = reader Deque.tail

{-|
/O(1)/, occasionally /O(n)/.
Keep all elements but the last one.
-}
init :: MonadReader (Deque a) m => m (Deque a)
init = reader Deque.init
