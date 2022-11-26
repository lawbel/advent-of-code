module Test15 where

import Data.Word (Word64)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day15 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

seedA :: Word64
seedA = 65

seedB :: Word64
seedB = 8921

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "first five gen. A" $
        let actual = take 5 $ drop 1 $ iterate (Day15.next Day15.genA) seedA
            expect = [1092455, 1181022009, 245556042, 1744312007, 1352636452]
        in  actual @?= expect
    , HUnit.testCase "first five gen. B" $
        let actual = take 5 $ drop 1 $ iterate (Day15.next Day15.genB) seedB
            expect = [430625591, 1233683848, 1431495498, 137874439, 285222916]
        in  actual @?= expect
    , HUnit.testCase "matches in first five" $
        Day15.judge 5 (Day15.genA, seedA) (Day15.genB, seedB) @?= 1
    , HUnit.testCase "matches in first forty million" $
        Day15.judge 40_000_000 (Day15.genA, seedA) (Day15.genB, seedB)
            @?= 588 ]

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "first five new gen. A" $
        let actual = take 5 $ drop 1 $ iterate (Day15.next Day15.genA') seedA
            expect = [1352636452, 1992081072, 530830436, 1980017072, 740335192]
        in  actual @?= expect
    , HUnit.testCase "first five new gen. B" $
        let actual = take 5 $ drop 1 $ iterate (Day15.next Day15.genB') seedB
            expect = [1233683848, 862516352, 1159784568, 1616057672, 412269392]
        in  actual @?= expect
    , HUnit.testCase "matches in first five" $
        Day15.judge 5 (Day15.genA', seedA) (Day15.genB', seedB) @?= 0
    , HUnit.testCase "matches in first five million" $
        Day15.judge 5_000_000 (Day15.genA', seedA) (Day15.genB', seedB)
            @?= 309 ]
