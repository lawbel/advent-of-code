{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test08 where

import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Hedgehog (Gen, (===))
import Hedgehog qualified as Hedge
import Hedgehog.Gen qualified as Hedge.Gen
import Hedgehog.Range qualified as Hedge.Range
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty.Hedge
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Parse

import Day08 qualified
import Day08 (Condition(..), Instr)


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests, propTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

genValue :: Gen Int
genValue = Hedge.Gen.integral $ Hedge.Range.constantFrom 100 (-1000) 1000

-- | We make sure this generator will produce a lot of repeats, to avoid an
-- outcome where the instructions we generate all refer to different registers
-- and thus have no interaction with one another.
genName :: Gen Text
genName = Text.singleton <$> Hedge.Gen.element ['a'..'f']

genCondition :: Gen l -> Gen r -> Gen (Condition l r)
genCondition genL genR = do
    left <- genL
    right <- genR
    Hedge.Gen.element
        [ left :<:  right
        , left :<=: right
        , left :>:  right
        , left :>=: right
        , left :==: right
        , left :!=: right ]

genInstrs :: Gen a -> Gen b -> Gen (Instr a b)
genInstrs genA genB = do
    register <- genA
    operator <- Hedge.Gen.element [Day08.Inc, Day08.Dec]
    value <- genB
    condition <- genCondition genA genB
    pure $ Day08.MkInstr{..}

propTests :: TestTree
propTests = Tasty.testGroup "property tests"
    [ Tasty.Hedge.testProperty "evalInstrs == fst . evalInstrsTraceMax" $
        Hedge.property $ do
            instrs <- fmap Vec.fromList $ Hedge.forAll $
                Hedge.Gen.list (Hedge.Range.linear 1 100) $
                    genInstrs genName genValue
            Day08.evalInstrs instrs HashMap.empty ===
                fst (Day08.evalInstrsTraceMax instrs HashMap.empty 0) ]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "can parse instructions" $
        Parse.runParser (Day08.parseInstrs <* Parse.eof) "test" exampleText
            @?= Right exampleInstrs
    , HUnit.testCase "can process instructions" $
        Day08.evalInstrs exampleInstrs HashMap.empty
            @?= HashMap.fromList [("a", 1), ("c", -10)] ]

exampleText :: Text
exampleText =
    "b inc 5 if a > 1\n\
    \a inc 1 if b < 5\n\
    \c dec -10 if a >= 1\n\
    \c inc -20 if c == 10\n"

exampleInstrs :: Vector (Instr Text Int)
exampleInstrs = Vec.fromList
    [ Day08.MkInstr "b" Day08.Inc 5     ("a" :>: 1)
    , Day08.MkInstr "a" Day08.Inc 1     ("b" :<: 5)
    , Day08.MkInstr "c" Day08.Dec (-10) ("a" :>=: 1)
    , Day08.MkInstr "c" Day08.Inc (-20) ("c" :==: 10) ]

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "can track max value" $
        snd (Day08.evalInstrsTraceMax exampleInstrs HashMap.empty 0)
            @?= 10 ]
