{-|
Definitions of strict Deque.

The typical `toList` and `fromList` conversions are provided by means of
the `Foldable` and `IsList` instances.
-}
module Deque.Strict
(
  StrictDefs.Deque,
  fromLazy,
  toLazy,
  StrictDefs.fromConsAndSnocLists,
  StrictDefs.cons,
  StrictDefs.snoc,
  StrictDefs.reverse,
  StrictDefs.shiftLeft,
  StrictDefs.shiftRight,
  StrictDefs.filter,
  StrictDefs.take,
  StrictDefs.takeWhile,
  StrictDefs.dropWhile,
  StrictDefs.uncons,
  StrictDefs.unsnoc,
  StrictDefs.null,
  StrictDefs.head,
  StrictDefs.last,
  StrictDefs.tail,
  StrictDefs.init,
)
where

import Deque.Prelude
import qualified Deque.Lazy.Defs as LazyDefs
import qualified Deque.Strict.Defs as StrictDefs

{-| Convert lazy deque to strict deque. -}
fromLazy :: LazyDefs.Deque a -> StrictDefs.Deque a
fromLazy (LazyDefs.Deque consList snocList) = StrictDefs.Deque (fromList consList) (fromList snocList)

{-| Convert strict deque to lazy deque. -}
toLazy :: StrictDefs.Deque a -> LazyDefs.Deque a
toLazy (StrictDefs.Deque consList snocList) = LazyDefs.Deque (toList consList) (toList snocList)
