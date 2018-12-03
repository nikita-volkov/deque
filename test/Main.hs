module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck
import qualified Deque as Deque
import qualified Data.List as List


main =
  defaultMain $
  testGroup "List roundtrips" $ let
  testPredicateProperty name listOp dequeOp =
    testProperty name $ \ (list1 :: [Int], list2 :: [Int], threshold :: Int, direction :: Int) -> let
      compare = case mod direction 5 of
        0 -> (>)
        1 -> (>=)
        2 -> (==)
        3 -> (<=)
        4 -> (<)
      predicate = compare threshold
      in
        listOp predicate (list1 <> list2) ===
        toList (dequeOp predicate (Deque.Deque (reverse list2) list1))
    in [
        testPredicateProperty "takeWhile" List.takeWhile Deque.takeWhile
        ,
        testPredicateProperty "dropWhile" List.dropWhile Deque.dropWhile
      ]
