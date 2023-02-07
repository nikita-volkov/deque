-- |
-- Lazy Deque API lifted to a State monad, \"mtl\"-style.
module Deque.Lazy.State where

import Deque.Lazy (Deque)
import qualified Deque.Lazy as Deque
import Deque.Prelude hiding (dropWhile, head, init, last, null, reverse, tail, takeWhile)
import qualified Deque.Prelude as Prelude

-- |
-- \(\mathcal{O}(n)\).
-- Modify each element of the queue.
map :: MonadState (Deque a) m => (a -> a) -> m ()
map f = modify (fmap f)

-- |
-- \(\mathcal{O}(n)\).
-- Add elements to the begginning.
prepend :: MonadState (Deque a) m => Deque a -> m ()
prepend deque = modify (deque <>)

-- |
-- \(\mathcal{O}(n)\).
-- Add elements to the ending.
append :: MonadState (Deque a) m => Deque a -> m ()
append deque = modify (<> deque)

-- |
-- \(\mathcal{O}(1)\).
-- Add element in the beginning.
cons :: MonadState (Deque a) m => a -> m ()
cons a = modify (Deque.cons a)

-- |
-- \(\mathcal{O}(1)\).
-- Add element in the ending.
snoc :: MonadState (Deque a) m => a -> m ()
snoc a = modify (Deque.snoc a)

-- |
-- \(\mathcal{O}(1)\).
-- Reverse the deque.
reverse :: MonadState (Deque a) m => m ()
reverse = modify Deque.reverse

-- |
-- \(\mathcal{O}(1)\), occasionally \(\mathcal{O}(n)\).
-- Move the first element to the end.
shiftLeft :: MonadState (Deque a) m => m ()
shiftLeft = modify Deque.shiftLeft

-- |
-- \(\mathcal{O}(1)\), occasionally \(\mathcal{O}(n)\).
-- Move the last element to the beginning.
shiftRight :: MonadState (Deque a) m => m ()
shiftRight = modify Deque.shiftRight

-- |
-- \(\mathcal{O}(n)\).
-- Leave only the elements satisfying the predicate.
filter :: MonadState (Deque a) m => (a -> Bool) -> m ()
filter predicate = modify (Deque.filter predicate)

-- |
-- \(\mathcal{O}(n)\).
-- Leave only the specified amount of first elements.
take :: MonadState (Deque a) m => Int -> m ()
take = modify . Deque.take

-- |
-- \(\mathcal{O}(n)\).
-- Drop the specified amount of first elements.
drop :: MonadState (Deque a) m => Int -> m ()
drop = modify . Deque.drop

-- |
-- \(\mathcal{O}(n)\).
-- Leave only the first elements satisfying the predicate.
takeWhile :: MonadState (Deque a) m => (a -> Bool) -> m ()
takeWhile predicate = modify (Deque.takeWhile predicate)

-- |
-- \(\mathcal{O}(n)\).
-- Drop the first elements satisfying the predicate.
dropWhile :: MonadState (Deque a) m => (a -> Bool) -> m ()
dropWhile predicate = modify (Deque.dropWhile predicate)

-- |
-- \(\mathcal{O}(n)\).
-- Return the first elements satisfying the predicate, removing them from the state.
span :: MonadState (Deque a) m => (a -> Bool) -> m (Deque a)
span predicate = state (Deque.span predicate)

-- |
-- \(\mathcal{O}(1)\), occasionally \(\mathcal{O}(n)\).
-- Get the first element if deque is not empty,
-- removing the element.
uncons :: MonadState (Deque a) m => m (Maybe a)
uncons =
  state
    ( \deque -> case Deque.uncons deque of
        Nothing -> (Nothing, deque)
        Just (a, newDeque) -> (Just a, newDeque)
    )

-- |
-- \(\mathcal{O}(1)\), occasionally \(\mathcal{O}(n)\).
-- Get the last element if deque is not empty,
-- removing the element.
unsnoc :: MonadState (Deque a) m => m (Maybe a)
unsnoc =
  state
    ( \deque -> case Deque.unsnoc deque of
        Nothing -> (Nothing, deque)
        Just (a, newDeque) -> (Just a, newDeque)
    )

-- |
-- \(\mathcal{O}(1)\).
-- Check whether deque is empty.
null :: MonadState (Deque a) m => m Bool
null = gets Deque.null

-- |
-- \(\mathcal{O}(1)\).
-- Check whether deque is empty.
length :: MonadState (Deque a) m => m Int
length = gets Prelude.length

-- |
-- \(\mathcal{O}(1)\), occasionally \(\mathcal{O}(n)\).
-- Get the first element if deque is not empty.
head :: MonadState (Deque a) m => m (Maybe a)
head = gets Deque.head

-- |
-- \(\mathcal{O}(1)\), occasionally \(\mathcal{O}(n)\).
-- Get the last element if deque is not empty.
last :: MonadState (Deque a) m => m (Maybe a)
last = gets Deque.last

-- |
-- \(\mathcal{O}(1)\), occasionally \(\mathcal{O}(n)\).
-- Keep all elements but the first one.
tail :: MonadState (Deque a) m => m ()
tail = modify Deque.tail

-- |
-- \(\mathcal{O}(1)\), occasionally \(\mathcal{O}(n)\).
-- Keep all elements but the last one.
init :: MonadState (Deque a) m => m ()
init = modify Deque.init
