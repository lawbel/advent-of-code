module Test06 where

import Control.Monad (replicateM_)
import Control.Monad.ST qualified as ST
import Data.Vector.Unboxed qualified as Vec
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.InterpolatedString.Perl6 (qq)

import Day06 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests" $
    iterations <> [loopUntilRepeat]

iterations :: [TestTree]
iterations = do
    let initial = Vec.fromList [0, 2, 7, 0]
    (i, result) <- zip [1..]
        [ Vec.fromList [2, 4, 1, 2]
        , Vec.fromList [3, 1, 2, 3]
        , Vec.fromList [0, 2, 3, 4]
        , Vec.fromList [1, 3, 4, 1]
        , Vec.fromList [2, 4, 1, 2] ]
    pure $ HUnit.testCase [qq|after $i redistributions|] $
        redist initial i @?= result
  where
    redist initial n = ST.runST $ do
        memory <- Vec.thaw initial
        replicateM_ n $ Day06.redistribute memory
        Vec.freeze memory

loopUntilRepeat :: TestTree
loopUntilRepeat = HUnit.testCase "loop until repeat" $
    let initial = Vec.fromList [0, 2, 7, 0]
        result = ST.runST $ do
            memory <- Vec.thaw initial
            n <- fst <$> Day06.redistributeUntilRepeat memory
            frozen <- Vec.freeze memory
            pure (n, frozen)
    in  result @?= (5, Vec.fromList [2, 4, 1, 2])

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "loop until repeat" $ result @?= 4 ]
  where
    initial = Vec.fromList [0, 2, 7, 0]
    result = ST.runST $ do
        memory <- Vec.thaw initial
        snd <$> Day06.redistributeUntilRepeat memory
