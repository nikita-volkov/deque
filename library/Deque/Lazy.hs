{-|
Definitions of lazy Deque.

The typical `toList` and `fromList` conversions are provided by means of
the `Foldable` and `IsList` instances.
-}
module Deque.Lazy
(
  LazyDefs.Deque,
  fromStrict,
  toStrict,
  LazyDefs.fromConsAndSnocLists,
  LazyDefs.cons,
  LazyDefs.snoc,
  LazyDefs.reverse,
  LazyDefs.shiftLeft,
  LazyDefs.shiftRight,
  LazyDefs.filter,
  LazyDefs.take,
  LazyDefs.takeWhile,
  LazyDefs.dropWhile,
  LazyDefs.uncons,
  LazyDefs.unsnoc,
  LazyDefs.null,
  LazyDefs.head,
  LazyDefs.last,
  LazyDefs.tail,
  LazyDefs.init,
)
where

import Deque.Prelude
import qualified Deque.Lazy.Defs as LazyDefs
import qualified Deque.Strict.Defs as StrictDefs

{-| Convert strict deque to lazy deque. -}
fromStrict :: StrictDefs.Deque a -> LazyDefs.Deque a
fromStrict (StrictDefs.Deque consList snocList) = LazyDefs.Deque (toList consList) (toList snocList)

{-| Convert lazy deque to strict deque. -}
toStrict :: LazyDefs.Deque a -> StrictDefs.Deque a
toStrict (LazyDefs.Deque consList snocList) = StrictDefs.Deque (fromList consList) (fromList snocList)
