module CareerTrainer.Domain.Learning.Progress (
    AnswerOutcome (..),
    Progress,
    ProgressError (..),
    mkProgress,
    emptyProgress,
    applyAnswer,
    correctAnswers,
    totalAnswers,
    knowledgePercent,
    progressLevel,
) where

data AnswerOutcome
    = Correct
    | Incorrect
    deriving stock (Eq, Show)

data Progress = Progress
    { correctAnswers :: Int
    , totalAnswers :: Int
    }
    deriving stock (Eq, Show)

data ProgressError
    = NegativeCorrectAnswers
    | NegativeTotalAnswers
    | CorrectAnswersExceedTotal
    deriving stock (Eq, Show)

mkProgress :: Int -> Int -> Either ProgressError Progress
mkProgress correct total
    | correct < 0 =
        Left NegativeCorrectAnswers
    | total < 0 =
        Left NegativeTotalAnswers
    | correct > total =
        Left CorrectAnswersExceedTotal
    | otherwise =
        Right
            Progress
                { correctAnswers = correct
                , totalAnswers = total
                }

emptyProgress :: Progress
emptyProgress =
    Progress
        { correctAnswers = 0
        , totalAnswers = 0
        }

applyAnswer :: AnswerOutcome -> Progress -> Progress
applyAnswer outcome progress =
    Progress
        { correctAnswers =
            correctAnswers progress
                + case outcome of
                    Correct -> 1
                    Incorrect -> 0
        , totalAnswers =
            totalAnswers progress + 1
        }

knowledgePercent :: Progress -> Int
knowledgePercent progress
    | totalAnswers progress == 0 = 0
    | otherwise =
        clamp 0 100 percentage
  where
    percentage =
        fromInteger $
            toInteger (correctAnswers progress)
                * 100
                `div` toInteger (totalAnswers progress)

clamp :: (Ord a) => a -> a -> a -> a
clamp lower upper =
    min upper . max lower

progressLevel :: Progress -> Int
progressLevel progress =
    clamp
        1
        5
        (1 + knowledgePercent progress `div` 25)
