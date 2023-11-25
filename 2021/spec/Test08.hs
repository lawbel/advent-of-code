module Test08 where

import Data.Bifunctor (bimap)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
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
        let digits = Day08.outputs .> mapMaybe Day08.identifyByCount
        in  length (concatMap digits exampleEntries) @?= 26
    , HUnit.testCase "identify" $
        let expected = Map.fromList $ fmap swap
                [ (Day08.D8, Set.fromList [A, C, E, D, G, F, B])
                , (Day08.D5, Set.fromList [C, D, F, B, E])
                , (Day08.D2, Set.fromList [G, C, D, F, A])
                , (Day08.D3, Set.fromList [F, B, C, A, D])
                , (Day08.D7, Set.fromList [D, A, B])
                , (Day08.D9, Set.fromList [C, E, F, A, B, D])
                , (Day08.D6, Set.fromList [C, D, F, G, E, B])
                , (Day08.D4, Set.fromList [E, A, F, B])
                , (Day08.D0, Set.fromList [C, A, G, E, D, B])
                , (Day08.D1, Set.fromList [A, B])
                ]
            actual = Day08.identify (Day08.signals exampleEntry)
        in  fmap Day08.inverseMap actual @?= Just expected
    , Tasty.testGroup "identifyEntry"
        [ HUnit.testCase "first example" $
            Day08.identifyEntry exampleEntry @?= Just 5353
        , HUnit.testCase "larger example" $
            let output = Just <$>
                    [ 8394, 9781, 1197, 9361, 4873
                    , 8418, 4548, 1625, 8717, 4315
                    ]
            in  Day08.identifyEntry <$> exampleEntries @?= output
        ]
    ]

fromLists :: Ord a => [[a]] -> Set (Set a)
fromLists = fmap Set.fromList .> Set.fromList

makeEntry :: ([[Segment]], [[Segment]]) -> Entry
makeEntry = bimap (fmap Set.fromList) (fmap Set.fromList)
    .> uncurry Day08.MkEntry

exampleEntry :: Entry
exampleEntry = makeEntry
    ( [[A,C,E,D,G,F,B], [C,D,F,B,E], [G,C,D,F,A], [F,B,C,A,D], [D,A,B], [C,E,F,A,B,D], [C,D,F,G,E,B], [E,A,F,B], [C,A,G,E,D,B], [A,B]]
    , [[C,D,F,E,B], [F,C,A,D,B], [C,D,F,E,B], [C,D,B,A,F]]
    )

exampleEntries :: [Entry]
exampleEntries = fmap makeEntry
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
