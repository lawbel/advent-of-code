module Test04 where

import Control.Applicative (ZipList(ZipList), getZipList, liftA2)
import Data.Vector qualified as Vec
import Flow ((.>))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day04 (Matrix, Number)
import Day04 qualified
import Common


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ Tasty.testGroup "main example" $
        let example desc nums board win result = Tasty.testGroup desc $
                let asNums = n <<$>> board
                in  [ HUnit.testCase "drawNumbers" $
                        drawNumbers nums asNums @?= apply result board
                    , HUnit.testCase "isWinner" $
                        Day04.isWinner (drawNumbers nums asNums) @?= win
                    ]
        in  [ Tasty.testGroup "after five" $
                let nums = take 5 exampleNumbers
                in  [ example "board one" nums board1 False $ fromLists
                        [ [n, n, n, d, n]
                        , [n, n, n, d, n]
                        , [n, d, n, n, d]
                        , [n, n, n, n, d]
                        , [n, n, n, n, n]
                        ]
                    , example "board two" nums board2 False $ fromLists
                        [ [n, n, n, n, n]
                        , [d, n, n, n, d]
                        , [n, n, d, n, n]
                        , [n, d, n, n, d]
                        , [n, n, n, n, n]
                        ]
                    , example "board three" nums board3 False $ fromLists
                        [ [n, n, n, n, d]
                        , [n, n, n, d, n]
                        , [n, n, n, n, n]
                        , [n, d, n, n, d]
                        , [n, n, n, n, d]
                        ]
                    ]
            , Tasty.testGroup "after eleven" $
                let nums = take 11 exampleNumbers
                in  [ example "board one" nums board1 False $ fromLists
                        [ [n, n, d, d, d]
                        , [n, d, d, d, n]
                        , [d, d, d, n, d]
                        , [n, n, n, n, d]
                        , [n, n, n, n, n]
                        ]
                    , example "board two" nums board2 False $ fromLists
                        [ [n, n, d, d, n]
                        , [d, n, n, d, d]
                        , [n, n, d, n, d]
                        , [n, d, n, n, d]
                        , [d, d, n, n, n]
                        ]
                    , example "board three" nums board3 False $ fromLists
                        [ [d, d, d, n, d]
                        , [n, n, n, d, n]
                        , [n, n, d, n, n]
                        , [n, d, n, n, d]
                        , [d, d, n, n, d]
                        ]
                    ]
            , Tasty.testGroup "after twelve" $
                let nums = take 12 exampleNumbers
                in  [ example "board one" nums board1 False $ fromLists
                        [ [n, n, d, d, d]
                        , [n, d, d, d, d]
                        , [d, d, d, n, d]
                        , [n, n, n, n, d]
                        , [n, n, n, n, n]
                        ]
                    , example "board two" nums board2 False $ fromLists
                        [ [n, n, d, d, n]
                        , [d, n, n, d, d]
                        , [n, n, d, n, d]
                        , [n, d, n, d, d]
                        , [d, d, n, n, n]
                        ]
                    , example "board three" nums board3 True $ fromLists
                        [ [d, d, d, d, d]
                        , [n, n, n, d, n]
                        , [n, n, d, n, n]
                        , [n, d, n, n, d]
                        , [d, d, n, n, d]
                        ]
                    ]
            , Tasty.testGroup "playBingo" $
                let (lastCall, bingo) = Day04.playBingo
                        exampleNumbers
                        [board1, board2, board3]
                    boards = Day04.boardStates bingo
                in  [ HUnit.testCase "last number drawn" $
                        lastCall @?= Just 24
                    , HUnit.testCase "winning score" $
                        fmap Day04.score (filter Day04.isWinner boards)
                            @?= [188]
                    ]
            ]
    ]

exampleNumbers :: [Int]
exampleNumbers =
    [ 7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22
    , 18, 20, 8, 19, 3, 26, 1
    ]

board1 :: Matrix Int
board1 = fromLists
    [ [22, 13, 17, 11,  0]
    , [ 8,  2, 23,  4, 24]
    , [21,  9, 14, 16,  7]
    , [ 6, 10,  3, 18,  5]
    , [ 1, 12, 20, 15, 19]
    ]

board2 :: Matrix Int
board2 = fromLists
    [ [ 3, 15,  0,  2, 22]
    , [ 9, 18, 13, 17,  5]
    , [19,  8,  7, 25, 23]
    , [20, 11, 10, 24,  4]
    , [14, 21, 16, 12,  6]
    ]

board3 :: Matrix Int
board3 = fromLists
    [ [14, 21, 17, 24,  4]
    , [10, 16, 15,  9, 19]
    , [18,  8, 23, 26, 20]
    , [22, 11, 13,  6,  5]
    , [ 2,  0, 12,  3,  7]
    ]

apply :: Matrix (a -> b) -> Matrix a -> Matrix b
apply fs xs = fromZips $ liftA2 (<*>) (toZips fs) (toZips xs)
  where
    toZips   = toLists .> ZipList .> fmap ZipList
    fromZips = getZipList .> fmap getZipList .> fromLists

drawNumbers :: Eq a => [a] -> Matrix (Number a) -> Matrix (Number a)
drawNumbers xs mat = foldr Day04.drawNumber mat xs

fromLists :: [[a]] -> Matrix a
fromLists = Vec.fromList .> fmap Vec.fromList

toLists :: Matrix a -> [[a]]
toLists = Vec.toList .> fmap Vec.toList

d :: a -> Number a
d = (`Day04.MkNumber` Day04.Drawn)

n :: a -> Number a
n = (`Day04.MkNumber` Day04.NotDrawn)
