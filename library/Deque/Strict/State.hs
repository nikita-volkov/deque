{-|
Strict Deque API lifted to a State monad, \"mtl\"-style.
-}
module Deque.Strict.State
where

import Deque.Prelude hiding (tail, init, last, head, null, dropWhile, takeWhile, reverse)
import Deque.Strict (Deque)
import qualified Deque.Strict as Deque
import qualified Deque.Prelude as Prelude


{-|
/O(n)/.
Modify each element of the queue.
-}
map :: MonadState (Deque a) m => (a -> a) -> m ()
map f = modify (fmap f)

{-|
/O(n)/.
Add elements from the begginning.
-}
prepend :: MonadState (Deque a) m => Deque a -> m ()
prepend deque = modify (deque <>)

{-|
/O(n)/.
Add elements from the ending.
-}
append :: MonadState (Deque a) m => Deque a -> m ()
append deque = modify (<> deque)

{-|
/O(1)/.
Add element in the beginning.
-}
cons :: MonadState (Deque a) m => a -> m ()
cons a = modify (Deque.cons a)

{-|
/O(1)/.
Add element in the ending.
-}
snoc :: MonadState (Deque a) m => a -> m ()
snoc a = modify (Deque.snoc a)

{-|
/O(1)/.
Revert the deque.
-}
reverse :: MonadState (Deque a) m => m ()
reverse = modify Deque.reverse

{-|
/O(1)/, occasionally /O(n)/.
Move the first element to the end.
-}
shiftLeft :: MonadState (Deque a) m => m ()
shiftLeft = modify Deque.shiftLeft

{-|
/O(1)/, occasionally /O(n)/.
Move the last element to the beginning.
-}
shiftRight :: MonadState (Deque a) m => m ()
shiftRight = modify Deque.shiftRight

{-|
/O(n)/.
Leave only the elements satisfying the predicate.
-}
filter :: MonadState (Deque a) m => (a -> Bool) -> m ()
filter predicate = modify (Deque.filter predicate)

{-|
/O(n)/.
Leave only the first elements satisfying the predicate.
-}
takeWhile :: MonadState (Deque a) m => (a -> Bool) -> m ()
takeWhile predicate = modify (Deque.takeWhile predicate)

{-|
/O(n)/.
Drop the first elements satisfying the predicate.
-}
dropWhile :: MonadState (Deque a) m => (a -> Bool) -> m ()
dropWhile predicate = modify (Deque.dropWhile predicate)

{-|
/O(1)/, occasionally /O(n)/.
Get the first element and deque without it if it's not empty.
-}
uncons :: MonadState (Deque a) m => m (Maybe a)
uncons = state (\ deque -> case Deque.uncons deque of
  Nothing -> (Nothing, deque)
  Just (a, newDeque) -> (Just a, newDeque))

{-|
/O(1)/, occasionally /O(n)/.
Get the last element and deque without it if it's not empty.
-}
unsnoc :: MonadState (Deque a) m => m (Maybe a)
unsnoc = state (\ deque -> case Deque.unsnoc deque of
  Nothing -> (Nothing, deque)
  Just (a, newDeque) -> (Just a, newDeque))

{-|
/O(1)/. 
Check whether deque is empty.
-}
null :: MonadState (Deque a) m => m Bool
null = gets Deque.null

{-|
/O(1)/. 
Check whether deque is empty.
-}
length :: MonadState (Deque a) m => m Int
length = gets Prelude.length

{-|
/O(1)/, occasionally /O(n)/.
Get the first element if deque is not empty.
-}
head :: MonadState (Deque a) m => m (Maybe a)
head = gets Deque.head

{-|
/O(1)/, occasionally /O(n)/.
Get the last element if deque is not empty.
-}
last :: MonadState (Deque a) m => m (Maybe a)
last = gets Deque.last

{-|
/O(1)/, occasionally /O(n)/.
Keep all elements but the first one.

In case of empty deque returns an empty deque.
-}
tail :: MonadState (Deque a) m => m (Deque a)
tail = gets Deque.tail

{-|
/O(1)/, occasionally /O(n)/.
Keep all elements but the last one.

In case of empty deque returns an empty deque.
-}
init :: MonadState (Deque a) m => m (Deque a)
init = gets Deque.init
