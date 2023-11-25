module Test03 where

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.InterpolatedString.Perl6 (qq)

import Day03 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests" $ do
    (n, result) <- [(1, 0), (12, 3), (23, 2), (1024, 31)]
    pure $ HUnit.testCase [qq|square $n|] $
        Day03.manhattan (Day03.spiral !! pred n) @?= result
