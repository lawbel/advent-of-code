module Test05 where

import Control.Monad.ST qualified as ST
import Data.STRef qualified as STRef
import Data.Vector.Unboxed qualified as Vec
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day05 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "after 1 jump" $
        run 1 @?= Just (0, Vec.fromList [1, 3, 0, 1, -3])
    , HUnit.testCase "after 2 jumps" $
        run 2 @?= Just (1, Vec.fromList [2, 3, 0, 1, -3])
    , HUnit.testCase "after 3 jumps" $
        run 3 @?= Just (4, Vec.fromList [2, 4, 0, 1, -3])
    , HUnit.testCase "after 4 jumps" $
        run 4 @?= Just (1, Vec.fromList [2, 4, 0, 1, -2])
    , HUnit.testCase "after 5 jumps" $
        run 5 @?= Just (5, Vec.fromList [2, 5, 0, 1, -2])
    , HUnit.testCase "after 6 jumps" $
        run 6 @?= Nothing
    , HUnit.testCase "reach exit in 5 steps" $
        runUntilExit @?= 5 ]
  where
    initial = Vec.fromList @Int [0, 3, 0, 1, -3]
    run n = ST.runST $ do
        list <- Day05.MkList <$> Vec.thaw initial <*> STRef.newSTRef 0
        Day05.jumps n list >>= \case
            Just _ -> pure Nothing
            Nothing -> do
                ptr <- STRef.readSTRef $ Day05.ptr list
                frozen <- Vec.freeze $ Day05.list list
                pure $ Just (ptr, frozen)
    runUntilExit = ST.runST $ do
        list <- Day05.MkList <$> Vec.thaw initial <*> STRef.newSTRef 0
        Day05.jumpUntilExit list
