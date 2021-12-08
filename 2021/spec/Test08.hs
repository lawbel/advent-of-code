module Test08 where

import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Flow ((.>))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day08 (Entry, Segment(A, B, C, D, E, F, G))
import Day08 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ HUnit.testCase "identifyByCount" $
        let digits = Day08.outputValues .> mapMaybe Day08.identifyByCount
        in  length (concatMap digits exampleEntries) @?= 26
    ]

fromLists :: Ord a => [[a]] -> Set (Set a)
fromLists = fmap Set.fromList .> Set.fromList

exampleEntries :: [Entry]
exampleEntries = fmap mkEntry
    [ ( [[B,E], [C,F,B,E,G,A,D], [C,B,D,G,E,F], [F,G,A,E,C,D], [C,G,E,B], [F,D,C,G,E], [A,G,E,B,F,D], [F,E,C,D,B], [F,A,B,C,D], [E,D,B]]
      , [[F,D,G,A,C,B,E], [C,E,F,D,B], [C,E,F,B,G,D], [G,C,B,E]]
      )
    , ( [[E,D,B,F,G,A], [B,E,G,C,D], [C,B,G], [G,C], [G,C,A,D,E,B,F], [F,B,G,D,E], [A,C,B,G,F,D], [A,B,C,D,E], [G,F,C,B,E,D], [G,F,E,C]]
      , [[F,C,G,E,D,B], [C,G,B], [D,G,E,B,A,C,F], [G,C]]
      )
    , ( [[F,G,A,E,B,D], [C,G], [B,D,A,E,C], [G,D,A,F,B], [A,G,B,C,F,D], [G,D,C,B,E,F], [B,G,C,A,D], [G,F,A,C], [G,C,B], [C,D,G,A,B,E,F]]
      , [[C,G], [C,G], [F,D,C,A,G,B], [C,B,G]]
      )
    , ( [[F,B,E,G,C,D], [C,B,D], [A,D,C,E,F,B], [D,A,G,E,B], [A,F,C,B], [B,C], [A,E,F,D,C], [E,C,D,A,B], [F,G,D,E,C,A], [F,C,D,B,E,G,A]]
      , [[E,F,A,B,C,D], [C,E,D,B,A], [G,A,D,F,E,C], [C,B]]
      )
    , ( [[A,E,C,B,F,D,G], [F,B,G], [G,F], [B,A,F,E,G], [D,B,E,F,A], [F,C,G,E], [G,C,B,E,A], [F,C,A,E,G,B], [D,G,C,E,A,B], [F,C,B,D,G,A]]
      , [[G,E,C,F], [E,G,D,C,A,B,F], [B,G,F], [B,F,G,E,A]]
      )
    , ( [[F,G,E,A,B], [C,A], [A,F,C,E,B,G], [B,D,A,C,F,E,G], [C,F,A,E,D,G], [G,C,F,D,B], [B,A,E,C], [B,F,A,D,E,G], [B,A,F,G,C], [A,C,F]]
      , [[G,E,B,D,C,F,A], [E,C,B,A], [C,A], [F,A,D,E,G,C,B]]
      )
    , ( [[D,B,C,F,G], [F,G,D], [B,D,E,G,C,A,F], [F,G,E,C], [A,E,G,B,D,F], [E,C,D,F,A,B], [F,B,E,D,C], [D,A,C,G,B], [G,D,C,E,B,F], [G,F]]
      , [[C,E,F,G], [D,C,B,E,F], [F,C,G,E], [G,B,C,A,D,F,E]]
      )
    , ( [[B,D,F,E,G,C], [C,B,E,G,A,F], [G,E,C,B,F], [D,F,C,A,G,E], [B,D,A,C,G], [E,D], [B,E,D,F], [C,E,D], [A,D,C,B,E,F,G], [G,E,B,C,D]]
      , [[E,D], [B,C,G,A,F,E], [C,D,G,B,A], [C,B,G,E,F]]
      )
    , ( [[E,G,A,D,F,B], [C,D,B,F,E,G], [C,E,G,D], [F,E,C,A,B], [C,G,B], [G,B,D,E,F,C,A], [C,G], [F,G,C,D,A,B], [E,G,F,D,B], [B,F,C,E,G]]
      , [[G,B,D,F,C,A,E], [B,G,C], [C,G], [C,G,B]]
      )
    , ( [[G,C,A,F,B], [G,C,F], [D,C,A,E,B,F,G], [E,C,A,G,B], [G,F], [A,B,C,D,E,G], [G,A,E,F], [C,A,F,B,G,E], [F,D,B,A,C], [F,E,G,B,D,C]]
      , [[F,G,A,E], [C,F,G,A,B], [F,G], [B,A,G,C,E]]
      )
    ]
  where
    mkEntry = bimap (fmap Set.fromList) (fmap Set.fromList)
        .> uncurry Day08.MkEntry
