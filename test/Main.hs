module Main where

import qualified Data.List as List
import qualified Deque.Lazy as Lazy
import qualified Deque.Strict as Strict
import GHC.Exts as Exports (IsList (..))
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import qualified Test.QuickCheck.Property as QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude hiding (choose, toList)

main =
  defaultMain
    $ testGroup ""
    $ [ testImplementation
          "Strict"
          toList
          fromList
          Strict.fromConsAndSnocLists
          Strict.cons
          Strict.snoc
          Strict.reverse
          Strict.shiftLeft
          Strict.shiftRight
          Strict.filter
          Strict.take
          Strict.drop
          Strict.takeWhile
          Strict.dropWhile
          Strict.span
          Strict.uncons
          Strict.unsnoc
          Strict.null
          Strict.head
          Strict.last
          Strict.tail
          Strict.init,
        testImplementation
          "Lazy"
          toList
          fromList
          Lazy.fromConsAndSnocLists
          Lazy.cons
          Lazy.snoc
          Lazy.reverse
          Lazy.shiftLeft
          Lazy.shiftRight
          Lazy.filter
          Lazy.take
          Lazy.drop
          Lazy.takeWhile
          Lazy.dropWhile
          Lazy.span
          Lazy.uncons
          Lazy.unsnoc
          Lazy.null
          Lazy.head
          Lazy.last
          Lazy.tail
          Lazy.init,
        testGroup "Conversions"
          $ [ testGroup "Strict"
                $ [ testProperty "toLazy" $ forAll strictAndLazyDequeGen $ \(strictDeque, lazyDeque) ->
                      Strict.toLazy strictDeque === lazyDeque,
                    testProperty "fromLazy" $ forAll strictAndLazyDequeGen $ \(strictDeque, lazyDeque) ->
                      Strict.fromLazy lazyDeque === strictDeque
                  ],
              testGroup "Lazy"
                $ [ testProperty "toStrict" $ forAll strictAndLazyDequeGen $ \(strictDeque, lazyDeque) ->
                      Lazy.toStrict lazyDeque === strictDeque,
                    testProperty "fromStrict" $ forAll strictAndLazyDequeGen $ \(strictDeque, lazyDeque) ->
                      Lazy.fromStrict strictDeque === lazyDeque
                  ]
            ]
      ]

-- |
-- Test group, which abstracts over the implementation of deque.
testImplementation
  name
  (toList :: forall a. f a -> [a])
  fromList
  fromConsAndSnocLists
  cons
  snoc
  reverse
  shiftLeft
  shiftRight
  filter
  take
  drop
  takeWhile
  dropWhile
  span
  uncons
  unsnoc
  null
  head
  last
  tail
  init =
    testGroup ("Deque implementation: " <> name)
      $ [ testProperty "toList" $ forAll dequeAndListGen $ \(deque, list) ->
            toList deque === list,
          testProperty "fromList" $ forAll listGen $ \list ->
            toList (fromList list) === list,
          testProperty "eq" $ forAll dequeAndListGen $ \(deque, list) ->
            deque === fromList list,
          testProperty "show" $ forAll dequeAndListGen $ \(deque, list) ->
            show deque === show list,
          testProperty "cons" $ forAll ((,) <$> arbitrary <*> dequeAndListGen) $ \(a, (deque, list)) ->
            toList (cons a deque) === a : list,
          testProperty "snoc" $ forAll ((,) <$> arbitrary <*> dequeAndListGen) $ \(a, (deque, list)) ->
            toList (snoc a deque) === list <> [a],
          testProperty "reverse" $ forAll dequeAndListGen $ \(deque, list) ->
            toList (reverse deque) === List.reverse list,
          testProperty "shiftLeft" $ forAll dequeAndListGen $ \(deque, list) ->
            toList (shiftLeft deque) === List.drop 1 list <> List.take 1 list,
          testProperty "shiftRight" $ forAll dequeAndListGen $ \(deque, list) ->
            toList (shiftRight deque) === case list of
              [] -> []
              _ -> List.last list : List.init list,
          testProperty "filter" $ forAll ((,) <$> predicateGen <*> dequeAndListGen) $ \(predicate, (deque, list)) ->
            toList (filter predicate deque) === List.filter predicate list,
          testProperty "take" $ forAll ((,) <$> arbitrary <*> dequeAndListGen) $ \(amount, (deque, list)) ->
            toList (take amount deque) === List.take amount list,
          testProperty "drop" $ forAll ((,) <$> arbitrary <*> dequeAndListGen) $ \(amount, (deque, list)) ->
            toList (drop amount deque) === List.drop amount list,
          testProperty "takeWhile" $ forAll ((,) <$> predicateGen <*> dequeAndListGen) $ \(predicate, (deque, list)) ->
            toList (takeWhile predicate deque) === List.takeWhile predicate list,
          testProperty "dropWhile" $ forAll ((,) <$> predicateGen <*> dequeAndListGen) $ \(predicate, (deque, list)) ->
            toList (dropWhile predicate deque) === List.dropWhile predicate list,
          testProperty "span" $ forAll ((,) <$> predicateGen <*> dequeAndListGen) $ \(predicate, (deque, list)) ->
            bimap toList toList (span predicate deque) === List.span predicate list,
          testProperty "uncons" $ forAll dequeAndListGen $ \(deque, list) ->
            fmap (fmap toList) (uncons deque) === List.uncons list,
          testProperty "unsnoc" $ forAll dequeAndListGen $ \(deque, list) ->
            fmap (fmap toList) (unsnoc deque) === case list of
              [] -> Nothing
              _ -> Just (List.last list, List.init list),
          testProperty "null" $ forAll dequeAndListGen $ \(deque, list) ->
            null deque === List.null list,
          testProperty "head" $ forAll dequeAndListGen $ \(deque, list) ->
            head deque === case list of
              head : _ -> Just head
              _ -> Nothing,
          testProperty "last" $ forAll dequeAndListGen $ \(deque, list) ->
            last deque === case list of
              [] -> Nothing
              _ -> Just (List.last list),
          testProperty "tail" $ forAll dequeAndListGen $ \(deque, list) ->
            toList (tail deque) === case list of
              _ : tail -> tail
              _ -> [],
          testProperty "init" $ forAll dequeAndListGen $ \(deque, list) ->
            toList (init deque) === case list of
              [] -> []
              _ -> List.init list,
          testProperty "<>" $ forAll ((,) <$> dequeAndListGen <*> dequeAndListGen) $ \((deque1, list1), (deque2, list2)) ->
            toList (deque1 <> deque2) === (list1 <> list2),
          testProperty "<*>" $ forAll ((,) <$> dequeAndListGen <*> dequeAndListGen) $ \((deque1, list1), (deque2, list2)) ->
            toList ((,) <$> deque1 <*> deque2) === ((,) <$> list1 <*> list2),
          testProperty ">>=" $ forAll ((,) <$> dequeAndListKleisliGen <*> dequeAndListGen) $ \((dequeK, listK), (deque, list)) ->
            toList (deque >>= dequeK) === (list >>= listK),
          testProperty "foldl'" $ forAll dequeAndListGen $ \(deque, list) ->
            foldl' (flip (:)) [] deque === foldl' (flip (:)) [] list,
          testProperty "foldr" $ forAll dequeAndListGen $ \(deque, list) ->
            foldr (:) [] deque === foldr (:) [] list,
          testProperty "traverse" $ forAll dequeAndListGen $ \(deque, list) ->
            let fn x = if mod x 2 == 0 then Right x else Left x
             in fmap toList (traverse fn deque) === traverse fn list
        ]
    where
      dequeAndListGen = do
        consList <- listGen
        snocList <- listGen
        return (fromConsAndSnocLists consList snocList, consList <> List.reverse snocList)
      dequeAndListKleisliGen = do
        list <- listGen
        let listK x = fmap (+ x) list
            dequeK = fromList . listK
         in return (dequeK, listK)

sizedListGen maxSize = do
  length <- choose (0, maxSize)
  replicateM length (arbitrary @Word8)

listGen = arbitrary @[Word8]

predicateGen = do
  op <- elements [(>), (>=), (==), (<=), (<)]
  x <- arbitrary @Word8
  return (op x)

strictAndLazyDequeGen = do
  consList <- listGen
  snocList <- listGen
  return (Strict.fromConsAndSnocLists consList snocList, Lazy.fromConsAndSnocLists consList snocList)

-- * Workarounds to satisfy QuickCheck's requirements,

-- when we need to generate a predicate.
-------------------------

instance Show (Word8 -> Bool) where
  show _ = "@(Word8 -> Bool)"

instance Show (Word8 -> [Word8]) where
  show _ = "@(Word8 -> [Word8])"

instance Show (Word8 -> Strict.Deque Word8) where
  show _ = "@(Word8 -> Deque Word8)"

instance Show (Word8 -> Lazy.Deque Word8) where
  show _ = "@(Word8 -> Deque Word8)"
