module Main (main) where

import CareerTrainer.Domain.Learning.Progress qualified as Progress

main :: IO ()
main =
    case Progress.mkProgress 3 5 of
        Left err ->
            error ("No se pudo crear el progreso: " <> show err)
        Right currentProgress -> do
            let updatedProgress =
                    Progress.applyAnswer
                        Progress.Correct
                        currentProgress
            if Progress.correctAnswers updatedProgress == 4
                && Progress.totalAnswers updatedProgress == 6
                && Progress.knowledgePercent updatedProgress == 66
                && Progress.progressLevel updatedProgress == 3
                then
                    putStrLn " Progress test passed"
                else
                    error "Progress test failed"
