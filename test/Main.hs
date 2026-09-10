{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import CareerTrainer.Domain.Learning.Progress qualified as Progress
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (shrink), Gen, Property, QuickCheckTests (QuickCheckTests), chooseInt, conjoin, counterexample, elements, forAll, forAllShrink, once, testProperty, (===))

main :: IO ()
main =
    defaultMain tests

tests :: TestTree
tests =
    localOption (QuickCheckTests 1000) $
        testGroup
            "Progress"
            [ testProperty
                "mkProgress accepts valid boundary values"
                propMkProgressAcceptsBoundaries
            , testProperty
                "mkProgress rejects invalid states"
                propMkProgressRejectsInvalidStates
            , testProperty
                "Correct increments both counters"
                propCorrectIncrementsBothCounters
            , testProperty
                "Incorrect increments only total"
                propIncorrectIncrementsOnlyTotal
            , testProperty
                "Apply answers preserves invariant"
                propApplyAnswerPreservesInvariant
            , testProperty
                "Derived values remain in range"
                propDerivedValuesRemainInRange
            ]

data IncrementableProgressInput
    = IncrementableProgressInput Int Int
    deriving stock (Show)

genIncrementableProgressInput :: Gen IncrementableProgressInput
genIncrementableProgressInput = do
    total <- chooseInt (0, maxBound - 1)
    correct <- chooseInt (0, total)
    pure (IncrementableProgressInput correct total)

shrinkIncrementableProgressInput ::
    IncrementableProgressInput ->
    [IncrementableProgressInput]
shrinkIncrementableProgressInput
    (IncrementableProgressInput correct total) =
        [ IncrementableProgressInput smallerCorrect smallerTotal
        | (smallerCorrect, smallerTotal) <- shrink (correct, total)
        , smallerCorrect >= 0
        , smallerTotal >= 0
        , smallerCorrect < smallerTotal
        ]

genOutcome :: Gen Progress.AnswerOutcome
genOutcome =
    elements
        [ Progress.Correct
        , Progress.Incorrect
        ]

withProgress ::
    IncrementableProgressInput ->
    (Progress.Progress -> Property) ->
    Property
withProgress
    (IncrementableProgressInput correct total)
    assertion =
        case Progress.mkProgress correct total of
            Left err ->
                counterexample
                    ( "Generator produced invalid Progress: "
                        <> show err
                    )
                    False
            Right progress ->
                assertion progress

propMkProgressAcceptsBoundaries :: Property
propMkProgressAcceptsBoundaries =
    once $
        conjoin
            [ assertProgress 0 0
            , assertProgress 0 maxBound
            , assertProgress maxBound maxBound
            ]

propMkProgressRejectsInvalidStates :: Property
propMkProgressRejectsInvalidStates =
    once $
        conjoin
            [ Progress.mkProgress (-1) 0
                === Left Progress.NegativeCorrectAnswers
            , Progress.mkProgress 0 (-1)
                === Left Progress.NegativeTotalAnswers
            , Progress.mkProgress 2 1
                === Left Progress.CorrectAnswersExceedTotal
            ]

propCorrectIncrementsBothCounters :: Property
propCorrectIncrementsBothCounters =
    forAllShrink
        genIncrementableProgressInput
        shrinkIncrementableProgressInput
        $ \input@(IncrementableProgressInput correct total) ->
            withProgress input $ \progress ->
                let
                    updated =
                        Progress.applyAnswer
                            Progress.Correct
                            progress
                 in
                    conjoin
                        [ Progress.correctAnswers updated
                            === correct
                            + 1
                        , Progress.totalAnswers updated
                            === total
                            + 1
                        ]

propIncorrectIncrementsOnlyTotal :: Property
propIncorrectIncrementsOnlyTotal =
    forAllShrink
        genIncrementableProgressInput
        shrinkIncrementableProgressInput
        $ \input@(IncrementableProgressInput correct total) ->
            withProgress input $ \progress ->
                let
                    updated =
                        Progress.applyAnswer
                            Progress.Incorrect
                            progress
                 in
                    conjoin
                        [ Progress.correctAnswers updated
                            === correct
                        , Progress.totalAnswers updated
                            === total
                            + 1
                        ]

propApplyAnswerPreservesInvariant :: Property
propApplyAnswerPreservesInvariant =
    forAllShrink
        genIncrementableProgressInput
        shrinkIncrementableProgressInput
        $ \input ->
            forAll genOutcome $ \outcome ->
                withProgress input $ \progress ->
                    let
                        updated =
                            Progress.applyAnswer
                                outcome
                                progress
                        correct =
                            Progress.correctAnswers updated
                        total =
                            Progress.totalAnswers updated
                     in
                        conjoin
                            [ counterexample
                                "correctAnswers became negative"
                                (correct >= 0)
                            , counterexample
                                "totalAnswers became negative"
                                (total >= 0)
                            , counterexample
                                "correctAnswers exceeded totalAnswers"
                                (correct <= total)
                            ]

propDerivedValuesRemainInRange :: Property
propDerivedValuesRemainInRange =
    forAllShrink
        genIncrementableProgressInput
        shrinkIncrementableProgressInput
        $ \input ->
            withProgress input $ \progress ->
                let
                    knowledge =
                        Progress.knowledgePercent progress
                    level =
                        Progress.progressLevel progress
                 in
                    conjoin
                        [ counterexample
                            "knowledgePercent was below 0"
                            (knowledge >= 0)
                        , counterexample
                            "knowledgePercent exceeded 100"
                            (knowledge <= 100)
                        , counterexample
                            "progressLevel was below 1"
                            (level >= 1)
                        , counterexample
                            "progressLevel exceeded 5"
                            (level <= 5)
                        ]

assertProgress :: Int -> Int -> Property
assertProgress correct total =
    case Progress.mkProgress correct total of
        Left err ->
            counterexample
                ("mkProgress unexcepectedly failed: " <> show err)
                False
        Right progress ->
            conjoin
                [ Progress.correctAnswers progress
                    === correct
                , Progress.totalAnswers progress
                    === total
                ]
