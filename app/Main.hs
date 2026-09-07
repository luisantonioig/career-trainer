{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (
    FromJSON (parseJSON),
    ToJSON (toJSON),
    Value,
    eitherDecode,
    encode,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
 )
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Lucid
import Network.HTTP.Client hiding (withConnection)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (status204, status404)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv)
import Web.Scotty

import CareerTrainer.Domain.Learning.Progress qualified as Progress

data CareerGoal = CareerGoal
    { role :: Text
    , industry :: Text
    , mode :: Text
    , deadline :: Text
    , success :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

data LearningTopicInput = LearningTopicInput
    { inputTitle :: Text
    , inputDescription :: Text
    }

instance FromJSON LearningTopicInput where
    parseJSON =
        withObject "LearningTopicInput" $ \value ->
            LearningTopicInput
                <$> value .: "title"
                <*> value .:? "description" .!= ""

data LearningTopic = LearningTopic
    { topicId :: Int
    , topicTitle :: Text
    , topicDescription :: Text
    , topicLevel :: Int
    , topicKnowledge :: Int
    , topicCorrect :: Int
    , topicTotal :: Int
    }

instance FromRow LearningTopic where
    fromRow =
        LearningTopic
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

instance ToJSON LearningTopic where
    toJSON topic =
        object
            [ "id" .= topicId topic
            , "title" .= topicTitle topic
            , "description" .= topicDescription topic
            , "level" .= topicLevel topic
            , "knowledge" .= topicKnowledge topic
            , "correct" .= topicCorrect topic
            , "total" .= topicTotal topic
            ]

data LearningQuestion = LearningQuestion
    { questionId :: Int
    , questionTopicId :: Int
    , questionText :: Text
    , questionOptions :: [Text]
    , questionCorrectIndex :: Int
    , questionExplanation :: Text
    , questionDifficulty :: Int
    }

instance FromRow LearningQuestion where
    fromRow = do
        currentQuestionId <- field
        currentTopicId <- field
        currentQuestionText <- field
        optionsJson <- field
        currentCorrectIndex <- field
        currentExplanation <- field
        currentDifficulty <- field
        LearningQuestion
            <$> pure currentQuestionId
            <*> pure currentTopicId
            <*> pure currentQuestionText
            <*> pure (decodeOptions optionsJson)
            <*> pure currentCorrectIndex
            <*> pure currentExplanation
            <*> pure currentDifficulty

instance ToJSON LearningQuestion where
    toJSON question =
        object
            [ "id" .= questionId question
            , "topicId" .= questionTopicId question
            , "question" .= questionText question
            , "options" .= questionOptions question
            , "difficulty" .= questionDifficulty question
            ]

data GeneratedQuestion = GeneratedQuestion
    { generatedQuestion :: Text
    , generatedOptions :: [Text]
    , generatedCorrectIndex :: Int
    , generatedExplanation :: Text
    }

instance FromJSON GeneratedQuestion where
    parseJSON =
        withObject "GeneratedQuestion" $ \value ->
            GeneratedQuestion
                <$> value .: "question"
                <*> value .: "options"
                <*> value .: "correctIndex"
                <*> value .: "explanation"

data AnswerInput = AnswerInput
    { selectedIndex :: Int
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

data OpenAIResponse = OpenAIResponse
    { outputText :: Text
    }

data OpenAIOutput = OpenAIOutput
    { responseContent :: [OpenAIContent]
    }

data OpenAIContent = OpenAIContent
    { responseText :: Text
    }

instance FromJSON OpenAIResponse where
    parseJSON =
        withObject "OpenAIResponse" $ \value ->
            OpenAIResponse <$> do
                maybeTopLevelText <- value .:? "output_text"
                case maybeTopLevelText of
                    Just text -> pure text
                    Nothing -> do
                        outputs <- value .:? "output" .!= []
                        pure (firstOutputText outputs)

instance FromJSON OpenAIOutput where
    parseJSON =
        withObject "OpenAIOutput" $ \value -> do
            outputType <- value .:? "type" .!= ("" :: Text)
            if outputType == "message"
                then OpenAIOutput <$> value .:? "content" .!= []
                else pure (OpenAIOutput [])

instance FromJSON OpenAIContent where
    parseJSON =
        withObject "OpenAIContent" $ \value -> do
            contentType <- value .:? "type" .!= ("" :: Text)
            case contentType of
                "output_text" -> OpenAIContent <$> value .:? "text" .!= ""
                "refusal" -> OpenAIContent <$> value .:? "refusal" .!= ""
                _ -> pure (OpenAIContent "")

firstOutputText :: [OpenAIOutput] -> Text
firstOutputText outputs =
    case filter (not . Text.null) (concatMap (map responseText . responseContent) outputs) of
        text : _ -> text
        [] -> ""

instance FromRow CareerGoal where
    fromRow =
        CareerGoal
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field

instance ToRow CareerGoal where
    toRow goal =
        toRow
            ( role goal
            , industry goal
            , mode goal
            , deadline goal
            , success goal
            )

databasePath :: String
databasePath = "career-trainer.sqlite3"

main :: IO ()
main = do
    loadDotEnv
    initializeDatabase
    scotty 3000 $ do
        middleware logStdoutDev

        get "/" $
            htmlPage dashboard

        get "/objetivo" $
            htmlPage goalPage

        get "/aprendizaje" $
            htmlPage learningPage

        get "/health" $
            json
                ( object
                    [ "status" .= ("ok" :: Text)
                    , "service" .= ("career-trainer" :: Text)
                    ]
                )

        get "/api/goal" $ do
            savedGoal <- liftIO readCareerGoal
            json (object ["goal" .= savedGoal])

        post "/api/goal" $ do
            goal <- jsonData
            liftIO (saveCareerGoal goal)
            json (object ["goal" .= goal])

        delete "/api/goal" $ do
            liftIO deleteCareerGoal
            status status204

        get "/api/learning/topics" $ do
            topics <- liftIO readLearningTopics
            json (object ["topics" .= topics])

        post "/api/learning/topics" $ do
            topicInput <- jsonData
            topic <- liftIO (createLearningTopic topicInput)
            json (object ["topic" .= topic])

        get "/api/learning/topics/:topicId" $ do
            currentTopicId <- pathParam "topicId"
            topic <- liftIO (readLearningTopic currentTopicId)
            json (object ["topic" .= topic])

        post "/api/learning/topics/:topicId/question" $ do
            currentTopicId <- pathParam "topicId"
            result <- liftIO (generateLearningQuestion currentTopicId)
            case result of
                Left message -> do
                    status status404
                    json (object ["error" .= message])
                Right question ->
                    json (object ["question" .= question])

        post "/api/learning/questions/:questionId/answer" $ do
            currentQuestionId <- pathParam "questionId"
            answer <- jsonData
            result <- liftIO (answerLearningQuestion currentQuestionId answer)
            case result of
                Left message -> do
                    status status404
                    json (object ["error" .= message])
                Right payload ->
                    json payload

        notFound $ do
            status status404
            htmlPage notFoundPage

withDatabase :: (Connection -> IO a) -> IO a
withDatabase = withConnection databasePath

initializeDatabase :: IO ()
initializeDatabase =
    withDatabase $ \connection -> do
        execute_
            connection
            "CREATE TABLE IF NOT EXISTS career_goal (id INTEGER PRIMARY KEY CHECK (id = 1), role TEXT NOT NULL, industry TEXT NOT NULL, mode TEXT NOT NULL, deadline TEXT NOT NULL, success TEXT NOT NULL)"
        execute_
            connection
            "CREATE TABLE IF NOT EXISTS learning_topics (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL, description TEXT NOT NULL, level INTEGER NOT NULL DEFAULT 1, knowledge INTEGER NOT NULL DEFAULT 0, correct_answers INTEGER NOT NULL DEFAULT 0, total_answers INTEGER NOT NULL DEFAULT 0, created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP)"
        execute_
            connection
            "CREATE TABLE IF NOT EXISTS learning_questions (id INTEGER PRIMARY KEY AUTOINCREMENT, topic_id INTEGER NOT NULL, question TEXT NOT NULL, options_json TEXT NOT NULL, correct_index INTEGER NOT NULL, explanation TEXT NOT NULL, difficulty INTEGER NOT NULL, answered INTEGER NOT NULL DEFAULT 0, selected_index INTEGER, created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP, FOREIGN KEY(topic_id) REFERENCES learning_topics(id))"

readCareerGoal :: IO (Maybe CareerGoal)
readCareerGoal =
    withDatabase $ \connection ->
        listToMaybe
            <$> query_
                connection
                "SELECT role, industry, mode, deadline, success FROM career_goal WHERE id = 1"

saveCareerGoal :: CareerGoal -> IO ()
saveCareerGoal goal =
    withDatabase $ \connection ->
        execute
            connection
            "INSERT INTO career_goal (id, role, industry, mode, deadline, success) VALUES (1, ?, ?, ?, ?, ?) ON CONFLICT(id) DO UPDATE SET role = excluded.role, industry = excluded.industry, mode = excluded.mode, deadline = excluded.deadline, success = excluded.success"
            goal

deleteCareerGoal :: IO ()
deleteCareerGoal =
    withDatabase $ \connection ->
        execute_ connection "DELETE FROM career_goal WHERE id = 1"

readLearningTopics :: IO [LearningTopic]
readLearningTopics =
    withDatabase $ \connection ->
        query_
            connection
            "SELECT id, title, description, level, knowledge, correct_answers, total_answers FROM learning_topics ORDER BY created_at DESC"

readLearningTopic :: Int -> IO (Maybe LearningTopic)
readLearningTopic currentTopicId =
    withDatabase $ \connection ->
        listToMaybe
            <$> query
                connection
                "SELECT id, title, description, level, knowledge, correct_answers, total_answers FROM learning_topics WHERE id = ?"
                (Only currentTopicId)

createLearningTopic :: LearningTopicInput -> IO LearningTopic
createLearningTopic topicInput =
    withDatabase $ \connection -> do
        execute
            connection
            "INSERT INTO learning_topics (title, description) VALUES (?, ?)"
            (inputTitle topicInput, inputDescription topicInput)
        insertedId <- lastInsertRowId connection
        topicsAfterInsert <-
            query
                connection
                "SELECT id, title, description, level, knowledge, correct_answers, total_answers FROM learning_topics WHERE id = ?"
                (Only insertedId) ::
                IO [LearningTopic]
        let Just topic = listToMaybe topicsAfterInsert
        pure topic

generateLearningQuestion :: Int -> IO (Either Text LearningQuestion)
generateLearningQuestion currentTopicId = do
    maybeTopic <- readLearningTopic currentTopicId
    case maybeTopic of
        Nothing -> pure (Left "Tema de aprendizaje no encontrado.")
        Just topic -> do
            generated <- requestOpenAIQuestion topic
            question <- saveLearningQuestion currentTopicId (topicLevel topic) generated
            pure (Right question)

saveLearningQuestion :: Int -> Int -> GeneratedQuestion -> IO LearningQuestion
saveLearningQuestion currentTopicId difficulty generated =
    withDatabase $ \connection -> do
        execute
            connection
            "INSERT INTO learning_questions (topic_id, question, options_json, correct_index, explanation, difficulty) VALUES (?, ?, ?, ?, ?, ?)"
            ( currentTopicId
            , generatedQuestion generated
            , Text.decodeUtf8 (LBS.toStrict (encode (generatedOptions generated)))
            , generatedCorrectIndex generated
            , generatedExplanation generated
            , difficulty
            )
        insertedId <- lastInsertRowId connection
        questionsAfterInsert <-
            query
                connection
                "SELECT id, topic_id, question, options_json, correct_index, explanation, difficulty FROM learning_questions WHERE id = ?"
                (Only insertedId) ::
                IO [LearningQuestion]
        let Just question = listToMaybe questionsAfterInsert
        pure question

answerLearningQuestion :: Int -> AnswerInput -> IO (Either Text Value)
answerLearningQuestion currentQuestionId answer =
    withDatabase $ \connection -> do
        questions <-
            query
                connection
                "SELECT id, topic_id, question, options_json, correct_index, explanation, difficulty FROM learning_questions WHERE id = ?"
                (Only currentQuestionId) ::
                IO [LearningQuestion]
        let maybeQuestion = listToMaybe questions
        case maybeQuestion of
            Nothing -> pure (Left "Pregunta no encontrada.")
            Just question -> do
                let isCorrect = selectedIndex answer == questionCorrectIndex question
                topics <-
                    query
                        connection
                        "SELECT id, title, description, level, knowledge, correct_answers, total_answers FROM learning_topics WHERE id = ?"
                        (Only (questionTopicId question)) ::
                        IO [LearningTopic]
                let maybeTopic = listToMaybe topics
                case maybeTopic of
                    Nothing ->
                        pure (Left "Tema de aprendizaje no encontrado")
                    Just topic -> do
                        let outcome =
                                if isCorrect
                                    then Progress.Correct
                                    else Progress.Incorrect
                        case Progress.mkProgress (topicCorrect topic) (topicTotal topic) of
                            Left _ ->
                                pure (Left "El progreso del nuevo tema es inválido")
                            Right currentProgress -> do
                                let updatedProgress =
                                        Progress.applyAnswer
                                            outcome
                                            currentProgress

                                execute
                                    connection
                                    "UPDATE learning_questions SET answered = 1, selected_index = ? WHERE id = ?"
                                    (selectedIndex answer, currentQuestionId)

                                execute
                                    connection
                                    "UPDATE learning_topics SET correct_answers = ?, total_answers = ?, knowledge = ?, level = ? WHERE id = ?"
                                    ( Progress.correctAnswers updatedProgress
                                    , Progress.totalAnswers updatedProgress
                                    , Progress.knowledgePercent updatedProgress
                                    , Progress.progressLevel updatedProgress
                                    , questionTopicId question
                                    )

                                updatedTopics <-
                                    query
                                        connection
                                        "SELECT id, title, description, level, knowledge, correct_answers, total_answers FROM learning_topics where id = ?"
                                        (Only (questionTopicId question)) ::
                                        IO [LearningTopic]

                                let Just updatedTopic =
                                        listToMaybe updatedTopics
                                pure $
                                    Right $
                                        object
                                            [ "correct" .= isCorrect
                                            , "correctIndex"
                                                .= questionCorrectIndex question
                                            , "explanation"
                                                .= questionExplanation question
                                            , "topic" .= updatedTopic
                                            ]

-- execute
--     connection
--     "UPDATE learning_questions SET answered = 1, selected_index = ? WHERE id = ?"
--     (selectedIndex answer, currentQuestionId)
-- execute
--     connection
--     "UPDATE learning_topics SET total_answers = total_answers + 1, correct_answers = correct_answers + ?, knowledge = min(100, max(0, CAST(((correct_answers + ?) * 100.0 / (total_answers + 1)) AS INTEGER))), level = min(5, max(1, CAST(1 + (((correct_answers + ?) * 100.0 / (total_answers + 1)) / 25) AS INTEGER))) WHERE id = ?"
--     (if isCorrect then (1 :: Int) else 0, if isCorrect then (1 :: Int) else 0, if isCorrect then (1 :: Int) else 0, questionTopicId question)
-- updatedTopics <-
--     query
--         connection
--         "SELECT id, title, description, level, knowledge, correct_answers, total_answers FROM learning_topics WHERE id = ?"
--         (Only (questionTopicId question)) ::
--         IO [LearningTopic]
-- let Just topic = listToMaybe updatedTopics
-- pure $
--     Right $
--         object
--             [ "correct" .= isCorrect
--             , "correctIndex" .= questionCorrectIndex question
--             , "explanation" .= questionExplanation question
--             , "topic" .= topic
--             ]

requestOpenAIQuestion :: LearningTopic -> IO GeneratedQuestion
requestOpenAIQuestion topic = do
    maybeApiKey <- lookupEnv "OPENAI_API_KEY"
    case maybeApiKey of
        Nothing ->
            pure (fallbackQuestion topic)
        Just apiKey -> do
            model <- maybe "gpt-5.6" Text.pack <$> lookupEnv "OPENAI_MODEL"
            manager <- newManager tlsManagerSettings
            initialRequest <- parseRequest "https://api.openai.com/v1/responses"
            let request =
                    initialRequest
                        { method = "POST"
                        , requestHeaders =
                            [ ("Authorization", BS.pack ("Bearer " <> apiKey))
                            , ("Content-Type", "application/json")
                            ]
                        , requestBody = RequestBodyLBS (encode (openAIQuestionRequest model topic))
                        }
            result <- try (httpLbs request manager) :: IO (Either SomeException (Response LBS.ByteString))
            case result of
                Left _ -> pure (openAIFailureQuestion topic)
                Right response ->
                    case eitherDecode (responseBody response) of
                        Right parsed
                            | not (Text.null (outputText parsed)) ->
                                case eitherDecode (LBS.fromStrict (Text.encodeUtf8 (outputText parsed))) of
                                    Right generated -> pure generated
                                    Left _ -> pure (openAIFailureQuestion topic)
                        _ -> pure (openAIFailureQuestion topic)

openAIQuestionRequest :: Text -> LearningTopic -> Value
openAIQuestionRequest model topic =
    object
        [ "model" .= model
        , "input"
            .= [ object
                    [ "role" .= ("system" :: Text)
                    , "content" .= ("Genera una pregunta de opcion multiple en espanol para evaluar aprendizaje profesional. Responde solo con JSON valido que siga el esquema." :: Text)
                    ]
               , object
                    [ "role" .= ("user" :: Text)
                    , "content" .= learningPrompt topic
                    ]
               ]
        , "text"
            .= object
                [ "format"
                    .= object
                        [ "type" .= ("json_schema" :: Text)
                        , "name" .= ("learning_question" :: Text)
                        , "strict" .= True
                        , "schema" .= learningQuestionSchema
                        ]
                ]
        ]

learningPrompt :: LearningTopic -> Text
learningPrompt topic =
    Text.unlines
        [ "Tema: " <> topicTitle topic
        , "Descripcion: " <> topicDescription topic
        , "Nivel actual del usuario: " <> Text.pack (show (topicLevel topic)) <> " de 5."
        , "Porcentaje de conocimiento estimado: " <> Text.pack (show (topicKnowledge topic)) <> "%."
        , "Genera una pregunta ajustada a ese nivel. Debe tener 4 opciones y exactamente una respuesta correcta."
        ]

learningQuestionSchema :: Value
learningQuestionSchema =
    object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
                [ "question" .= object ["type" .= ("string" :: Text)]
                , "options"
                    .= object
                        [ "type" .= ("array" :: Text)
                        , "items" .= object ["type" .= ("string" :: Text)]
                        ]
                , "correctIndex" .= object ["type" .= ("integer" :: Text)]
                , "explanation" .= object ["type" .= ("string" :: Text)]
                ]
        , "required" .= (["question", "options", "correctIndex", "explanation"] :: [Text])
        , "additionalProperties" .= False
        ]

fallbackQuestion :: LearningTopic -> GeneratedQuestion
fallbackQuestion topic =
    GeneratedQuestion
        { generatedQuestion = "Sin OPENAI_API_KEY configurada. Pregunta de prueba para el tema: " <> topicTitle topic <> ". Que accion demuestra mejor dominio progresivo?"
        , generatedOptions =
            [ "Responder al azar y avanzar rapido"
            , "Practicar, recibir feedback y ajustar el plan"
            , "Leer una sola vez sin aplicar"
            , "Evitar preguntas dificiles"
            ]
        , generatedCorrectIndex = 1
        , generatedExplanation = "El dominio aumenta cuando practicas, recibes feedback y ajustas tu estrategia con evidencia."
        }

openAIFailureQuestion :: LearningTopic -> GeneratedQuestion
openAIFailureQuestion topic =
    GeneratedQuestion
        { generatedQuestion = "OPENAI_API_KEY fue detectada, pero no se pudo obtener una pregunta de OpenAI para " <> topicTitle topic <> ". Que conviene revisar primero?"
        , generatedOptions =
            [ "Que el servidor se haya reiniciado despues de definir la variable"
            , "Ignorar el error y seguir respondiendo al azar"
            , "Borrar la base de datos"
            , "Cambiar el objetivo laboral"
            ]
        , generatedCorrectIndex = 0
        , generatedExplanation = "Si la variable existe pero la llamada falla, lo primero es revisar reinicio del proceso, conectividad, modelo configurado y validez de la API key."
        }

loadDotEnv :: IO ()
loadDotEnv = do
    exists <- doesFileExist ".env"
    if exists
        then do
            contents <- readFile ".env"
            mapM_ loadDotEnvLine (lines contents)
        else pure ()

loadDotEnvLine :: String -> IO ()
loadDotEnvLine rawLine =
    case parseDotEnvLine rawLine of
        Nothing -> pure ()
        Just (key, value) -> do
            current <- lookupEnv key
            case current of
                Just _ -> pure ()
                Nothing -> setEnv key value

parseDotEnvLine :: String -> Maybe (String, String)
parseDotEnvLine rawLine =
    let line = trim rawLine
     in case line of
            "" -> Nothing
            '#' : _ -> Nothing
            _ ->
                let (key, valueWithEquals) = break (== '=') line
                 in case valueWithEquals of
                        '=' : value ->
                            Just (trim key, unquote (trim value))
                        _ -> Nothing

trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack

unquote :: String -> String
unquote value =
    case value of
        '"' : rest
            | not (null rest) && last rest == '"' -> init rest
        '\'' : rest
            | not (null rest) && last rest == '\'' -> init rest
        _ -> value

decodeOptions :: Text -> [Text]
decodeOptions rawOptions =
    case eitherDecode (LBS.fromStrict (Text.encodeUtf8 rawOptions)) of
        Right options -> options
        Left _ -> []

htmlPage :: Html () -> ActionM ()
htmlPage page = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    raw (Lucid.renderBS page)

dashboard :: Html ()
dashboard = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        title_ "Career Trainer"
        style_ stylesheet
    body_ $ do
        appHeader
        main_ $ do
            section_ [class_ "app-page"] $ do
                div_ [class_ "page-heading"] $ do
                    p_ [class_ "eyebrow"] "Panel principal"
                    h1_ "Entrenamiento laboral"
                    p_ [class_ "lead"] "Organiza tu objetivo, practica temas clave con IA y revisa avances reales para competir por mejores oportunidades."
                div_ [class_ "dashboard-grid"] $ do
                    a_ [class_ "module-card", href_ "/objetivo"] $ do
                        span_ [class_ "metric"] "Objetivo"
                        h2_ "Dirección laboral"
                        p_ "Define rol, industria, modalidad, plazo y criterios de éxito."
                    a_ [class_ "module-card", href_ "/aprendizaje"] $ do
                        span_ [class_ "metric"] "IA adaptativa"
                        h2_ "Aprendizaje"
                        p_ "Crea temas, responde preguntas y deja que el nivel suba o baje con tu desempeño."
                    article_ [class_ "module-card"] $ do
                        span_ [class_ "metric"] "Seguimiento"
                        h2_ "Revisión"
                        p_ "Conserva notas, decisiones y evidencia útil para ajustar tu preparación."

goalPage :: Html ()
goalPage = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        title_ "Objetivo laboral | Career Trainer"
        style_ stylesheet
    body_ $ do
        appHeader
        main_ $ do
            section_ [class_ "goal-section app-page"] $ do
                div_ [class_ "goal-copy"] $ do
                    p_ [class_ "eyebrow"] "Objetivo laboral"
                    h1_ "Dirección laboral"
                    p_ "Define el resultado que debe guiar tu preparación. Esta información se guarda en SQLite y alimenta las siguientes decisiones del entrenamiento."
                    div_ [class_ "goal-summary", id_ "goal-summary"] $ do
                        span_ [class_ "metric"] "Objetivo activo"
                        p_ [id_ "goal-summary-text"] "Aun no has definido un objetivo laboral."
                form_ [class_ "goal-form", id_ "goal-form"] $ do
                    label_ $ do
                        span_ "Rol objetivo"
                        input_ [type_ "text", id_ "goal-role", name_ "role", placeholder_ "Ej. Backend Haskell Engineer"]
                    label_ $ do
                        span_ "Industria o tipo de empresa"
                        input_ [type_ "text", id_ "goal-industry", name_ "industry", placeholder_ "Ej. fintech, salud, consultoria"]
                    div_ [class_ "field-grid"] $ do
                        label_ $ do
                            span_ "Modalidad"
                            select_ [id_ "goal-mode", name_ "mode"] $ do
                                option_ [value_ "Remoto"] "Remoto"
                                option_ [value_ "Hibrido"] "Hibrido"
                                option_ [value_ "Presencial"] "Presencial"
                        label_ $ do
                            span_ "Plazo"
                            select_ [id_ "goal-deadline", name_ "deadline"] $ do
                                option_ [value_ "30 dias"] "30 dias"
                                option_ [value_ "60 dias"] "60 dias"
                                option_ [value_ "90 dias"] "90 dias"
                    label_ $ do
                        span_ "Criterios de exito"
                        textarea_ [id_ "goal-success", name_ "success", placeholder_ "Ej. conseguir 5 entrevistas calificadas y cerrar una oferta remota..."] ""
                    div_ [class_ "form-actions"] $ do
                        button_ [type_ "submit"] "Guardar objetivo"
                        button_ [type_ "button", class_ "ghost", id_ "goal-clear"] "Limpiar"
        script_ goalScript

learningPage :: Html ()
learningPage = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        title_ "Aprendizaje | Career Trainer"
        style_ stylesheet
    body_ $ do
        appHeader
        main_ $
            section_ [class_ "learning-page"] $ do
                div_ [class_ "learning-top"] $ do
                    div_ $ do
                        p_ [class_ "eyebrow"] "Aprendizaje"
                        h1_ "Centro de entrenamiento"
                        p_ "Crea temas, practica con preguntas generadas por OpenAI y mide dominio real por nivel, aciertos y porcentaje de conocimiento."
                    div_ [class_ "learning-actions"] $
                        button_ [type_ "button", id_ "quick-generate", disabled_ ""] "Generar pregunta"
                div_ [class_ "learning-stats"] $ do
                    div_ [class_ "stat-box"] $ do
                        span_ "Temas"
                        strong_ [id_ "stat-topics"] "0"
                    div_ [class_ "stat-box"] $ do
                        span_ "Conocimiento promedio"
                        strong_ [id_ "stat-knowledge"] "0%"
                    div_ [class_ "stat-box"] $ do
                        span_ "Respuestas"
                        strong_ [id_ "stat-answers"] "0"
                    div_ [class_ "stat-box"] $ do
                        span_ "Nivel activo"
                        strong_ [id_ "stat-level"] "-"
                div_ [class_ "learning-shell"] $ do
                    aside_ [class_ "learning-sidebar"] $ do
                        section_ [class_ "panel-block"] $ do
                            div_ [class_ "panel-title"] $ do
                                h2_ "Nuevo tema"
                                p_ "Define una habilidad o concepto que quieras dominar."
                            form_ [class_ "topic-form", id_ "topic-form"] $ do
                                label_ $ do
                                    span_ "Tema"
                                    input_ [type_ "text", id_ "topic-title", placeholder_ "Ej. Concurrencia en Haskell", required_ ""]
                                label_ $ do
                                    span_ "Enfoque"
                                    textarea_ [id_ "topic-description", placeholder_ "Conceptos, objetivos o contexto que quieres practicar..."] ""
                                button_ [type_ "submit"] "Agregar tema"
                        section_ [class_ "panel-block"] $ do
                            div_ [class_ "panel-title"] $ do
                                h2_ "Temas"
                                p_ "Selecciona uno para entrenar."
                            div_ [class_ "topic-list", id_ "topic-list"] ""
                    article_ [class_ "learning-workspace"] $ do
                        div_ [class_ "learning-empty", id_ "learning-empty"] $ do
                            h2_ "Selecciona un tema"
                            p_ "Al abrir un tema verás su nivel, porcentaje de conocimiento y una pregunta adaptada a tu desempeño."
                        div_ [class_ "question-panel hidden", id_ "question-panel"] $ do
                            div_ [class_ "practice-toolbar"] $ do
                                div_ $ do
                                    span_ [class_ "metric", id_ "active-topic-level"] "Nivel 1"
                                    h2_ [id_ "active-topic-title"] ""
                                    p_ [id_ "active-topic-progress"] ""
                                div_ [class_ "knowledge-card"] $ do
                                    span_ "Dominio"
                                    strong_ [id_ "active-topic-knowledge"] "0%"
                            div_ [class_ "practice-state"] $ do
                                span_ [class_ "state-dot"] ""
                                span_ [id_ "practice-state-text"] "Listo para generar pregunta"
                            div_ [class_ "question-card"] $ do
                                p_ [class_ "question-label"] "Pregunta actual"
                                p_ [class_ "question-text", id_ "question-text"] ""
                            div_ [class_ "answer-options", id_ "answer-options"] ""
                            div_ [class_ "practice-footer"] $ do
                                button_ [type_ "button", id_ "generate-question"] "Generar pregunta"
                                div_ [class_ "feedback", id_ "answer-feedback"] ""
        script_ learningScript

appHeader :: Html ()
appHeader =
    header_ [class_ "topbar"] $ do
        a_ [class_ "brand", href_ "/"] "Career Trainer"
        nav_ [class_ "nav"] $ do
            a_ [href_ "/"] "Panel"
            a_ [href_ "/objetivo"] "Objetivo"
            a_ [href_ "/aprendizaje"] "Aprendizaje"

notFoundPage :: Html ()
notFoundPage = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        title_ "Pagina no encontrada"
        style_ stylesheet
    body_ [class_ "centered"] $ do
        main_ [class_ "empty-state"] $ do
            h1_ "404"
            p_ "La ruta solicitada no existe."
            a_ [class_ "button primary", href_ "/"] "Volver al inicio"

stylesheet :: Text
stylesheet =
    Text.unlines
        [ ":root { color-scheme: light; --ink: #18202a; --muted: #5e6a76; --line: #d9e0e7; --surface: #f6f8fb; --accent: #0f766e; --accent-strong: #0b5d56; --warm: #f4b860; }"
        , "* { box-sizing: border-box; }"
        , "html { scroll-behavior: smooth; }"
        , "body { margin: 0; font-family: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", sans-serif; color: var(--ink); background: #ffffff; }"
        , "a { color: inherit; text-decoration: none; }"
        , ".topbar { position: sticky; top: 0; z-index: 10; display: flex; align-items: center; justify-content: space-between; gap: 24px; padding: 18px clamp(20px, 4vw, 56px); border-bottom: 1px solid var(--line); background: rgba(255, 255, 255, 0.92); backdrop-filter: blur(12px); }"
        , ".brand { font-weight: 800; font-size: 1.05rem; }"
        , ".nav { display: flex; gap: 18px; color: var(--muted); font-size: 0.95rem; }"
        , ".app-page { padding: 40px clamp(20px, 4vw, 56px) 72px; }"
        , ".page-heading { max-width: 840px; margin-bottom: 28px; }"
        , ".page-heading h1 { margin: 0; font-size: clamp(2rem, 4vw, 4rem); line-height: 1.02; max-width: 14ch; }"
        , ".dashboard-grid { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 18px; }"
        , ".module-card { min-height: 240px; display: grid; align-content: start; gap: 16px; padding: 24px; border: 1px solid var(--line); border-radius: 8px; background: var(--surface); }"
        , ".module-card h2 { margin: 0; font-size: 1.35rem; }"
        , ".module-card p { margin: 0; color: var(--muted); line-height: 1.65; }"
        , ".hero { min-height: 76vh; display: grid; grid-template-columns: minmax(0, 1.2fr) minmax(280px, 0.8fr); align-items: center; gap: clamp(28px, 5vw, 80px); padding: clamp(48px, 8vw, 96px) clamp(20px, 4vw, 56px); background: linear-gradient(135deg, #f8fafc 0%, #e8f3f1 54%, #fff4db 100%); }"
        , ".hero-content { max-width: 760px; }"
        , ".eyebrow { margin: 0 0 14px; font-weight: 800; color: var(--accent-strong); text-transform: uppercase; font-size: 0.78rem; letter-spacing: 0; }"
        , "h1 { margin: 0; font-size: clamp(2.25rem, 5vw, 5.5rem); line-height: 0.98; letter-spacing: 0; max-width: 11ch; }"
        , ".lead { max-width: 680px; margin: 24px 0 0; color: #374151; font-size: 1.16rem; line-height: 1.65; }"
        , ".actions { display: flex; flex-wrap: wrap; gap: 12px; margin-top: 30px; }"
        , ".button, button { min-height: 44px; display: inline-flex; align-items: center; justify-content: center; border-radius: 8px; padding: 0 18px; border: 1px solid transparent; font-weight: 750; cursor: pointer; }"
        , ".primary { color: #ffffff; background: var(--accent); }"
        , ".secondary { color: var(--ink); background: #ffffff; border-color: var(--line); }"
        , ".hero-panel { border-left: 4px solid var(--accent); padding: 28px; background: rgba(255, 255, 255, 0.72); box-shadow: 0 24px 80px rgba(15, 23, 42, 0.12); }"
        , ".hero-panel h2, .review h2, .track-card h2 { margin: 0; font-size: 1.35rem; }"
        , ".hero-panel ul { margin: 20px 0 0; padding-left: 20px; color: #344054; line-height: 1.9; }"
        , ".goal-section { display: grid; grid-template-columns: minmax(0, 0.85fr) minmax(320px, 1.15fr); gap: clamp(24px, 4vw, 56px); padding: 64px clamp(20px, 4vw, 56px); align-items: start; }"
        , ".goal-copy h1 { margin: 0; font-size: clamp(2rem, 4vw, 4rem); line-height: 1.02; max-width: 12ch; }"
        , ".goal-copy h2 { margin: 0; font-size: clamp(1.8rem, 3vw, 3rem); line-height: 1.08; max-width: 12ch; }"
        , ".goal-copy p { color: var(--muted); line-height: 1.65; max-width: 560px; }"
        , ".goal-summary { margin-top: 28px; padding: 22px; border: 1px solid var(--line); border-radius: 8px; background: var(--surface); }"
        , ".goal-summary p { margin: 0; color: #344054; }"
        , ".goal-form { display: grid; gap: 16px; padding: 24px; border: 1px solid var(--line); border-radius: 8px; background: #ffffff; box-shadow: 0 18px 56px rgba(15, 23, 42, 0.08); }"
        , "label { display: grid; gap: 8px; color: #344054; font-weight: 750; }"
        , "input, select, textarea { width: 100%; border: 1px solid var(--line); border-radius: 8px; padding: 13px 14px; font: inherit; color: var(--ink); background: #ffffff; }"
        , "input:focus, select:focus, textarea:focus { outline: 3px solid rgba(15, 118, 110, 0.16); border-color: var(--accent); }"
        , ".field-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 16px; }"
        , ".form-actions { display: flex; flex-wrap: wrap; gap: 12px; }"
        , ".ghost { color: var(--ink); background: #ffffff; border-color: var(--line); }"
        , ".learning-page { min-height: calc(100vh - 73px); padding: 32px clamp(20px, 4vw, 56px) 56px; background: #eef3f7; }"
        , ".learning-top { display: flex; justify-content: space-between; gap: 24px; align-items: end; margin-bottom: 18px; }"
        , ".learning-top h1 { margin: 0; max-width: none; font-size: clamp(2rem, 4vw, 3.6rem); line-height: 1.02; }"
        , ".learning-top p:not(.eyebrow) { max-width: 760px; margin: 14px 0 0; color: var(--muted); line-height: 1.6; }"
        , ".learning-actions { display: flex; gap: 10px; flex: 0 0 auto; }"
        , ".learning-actions button:disabled { cursor: not-allowed; opacity: 0.46; }"
        , ".learning-stats { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 12px; margin-bottom: 18px; }"
        , ".stat-box { min-height: 92px; display: grid; gap: 8px; align-content: center; padding: 18px; border: 1px solid var(--line); border-radius: 8px; background: #ffffff; }"
        , ".stat-box span { color: var(--muted); font-size: 0.9rem; }"
        , ".stat-box strong { font-size: 1.6rem; line-height: 1; }"
        , ".learning-shell { display: grid; grid-template-columns: minmax(320px, 0.36fr) minmax(0, 0.64fr); gap: 18px; align-items: start; }"
        , ".learning-sidebar { display: grid; gap: 14px; }"
        , ".panel-block, .learning-workspace { border: 1px solid var(--line); border-radius: 8px; background: #ffffff; box-shadow: 0 18px 48px rgba(15, 23, 42, 0.07); }"
        , ".panel-title { padding: 18px 20px 0; }"
        , ".panel-title h2 { margin: 0; font-size: 1.05rem; }"
        , ".panel-title p { margin: 6px 0 0; color: var(--muted); line-height: 1.5; font-size: 0.94rem; }"
        , ".topic-form { display: grid; gap: 14px; padding: 18px 20px 20px; }"
        , ".topic-list { display: grid; gap: 10px; padding: 14px; max-height: 520px; overflow: auto; }"
        , ".topic-item { width: 100%; display: grid; gap: 10px; min-height: 118px; padding: 14px; border: 1px solid var(--line); border-radius: 8px; color: var(--ink); background: #ffffff; text-align: left; }"
        , ".topic-item:hover { border-color: #9bb6b2; background: #f7fbfa; }"
        , ".topic-item.active { border-color: var(--accent); background: #edf7f5; box-shadow: inset 4px 0 0 var(--accent); }"
        , ".topic-item strong { font-size: 1rem; }"
        , ".topic-meta { display: flex; justify-content: space-between; gap: 10px; color: var(--muted); font-size: 0.9rem; }"
        , ".topic-progress { height: 9px; overflow: hidden; border-radius: 999px; background: #dfe7ec; }"
        , ".topic-progress span { display: block; height: 100%; background: linear-gradient(90deg, var(--accent), #3b82f6); }"
        , ".learning-workspace { min-height: 620px; padding: 0; overflow: hidden; }"
        , ".learning-empty { min-height: 620px; display: grid; place-items: center; padding: 36px; color: var(--muted); text-align: center; }"
        , ".learning-empty h2 { margin: 0 0 10px; color: var(--ink); }"
        , ".learning-empty p { max-width: 460px; margin: 0; line-height: 1.6; }"
        , ".hidden { display: none; }"
        , ".question-panel { display: grid; gap: 0; }"
        , ".practice-toolbar { display: flex; justify-content: space-between; gap: 18px; align-items: start; padding: 22px; border-bottom: 1px solid var(--line); background: #fbfcfd; }"
        , ".practice-toolbar h2 { margin: 6px 0 0; font-size: 1.45rem; }"
        , ".practice-toolbar p { margin: 8px 0 0; color: var(--muted); }"
        , ".knowledge-card { width: 112px; min-height: 92px; display: grid; place-items: center; gap: 4px; flex: 0 0 auto; border-radius: 8px; background: #10201f; color: #ffffff; }"
        , ".knowledge-card span { color: #a7c8c4; font-size: 0.8rem; }"
        , ".knowledge-card strong { font-size: 1.7rem; }"
        , ".practice-state { display: flex; align-items: center; gap: 10px; padding: 14px 22px; border-bottom: 1px solid var(--line); color: #344054; background: #ffffff; }"
        , ".state-dot { width: 10px; height: 10px; border-radius: 50%; background: var(--accent); box-shadow: 0 0 0 4px rgba(15, 118, 110, 0.12); }"
        , ".question-card { margin: 22px; padding: 22px; border: 1px solid var(--line); border-radius: 8px; background: #f8fafc; }"
        , ".question-label { margin: 0 0 10px; color: var(--muted); font-size: 0.84rem; font-weight: 800; text-transform: uppercase; }"
        , ".question-text { margin: 0; color: var(--ink); font-size: 1.08rem; line-height: 1.65; }"
        , ".answer-options { display: grid; gap: 10px; padding: 0 22px 22px; }"
        , ".answer-option { min-height: 56px; justify-content: flex-start; width: 100%; color: var(--ink); background: #ffffff; border-color: var(--line); text-align: left; line-height: 1.4; white-space: normal; }"
        , ".answer-option:hover { border-color: var(--accent); background: #f4fbfa; }"
        , ".answer-option.correct { border-color: #15803d; background: #ecfdf3; color: #14532d; }"
        , ".answer-option.incorrect { border-color: #b42318; background: #fff1f0; color: #7a271a; }"
        , ".practice-footer { display: flex; align-items: center; justify-content: space-between; gap: 18px; padding: 18px 22px 22px; border-top: 1px solid var(--line); }"
        , ".feedback { min-height: 32px; color: #344054; line-height: 1.55; }"
        , ".feedback.success { color: #14532d; }"
        , ".feedback.error { color: #7a271a; }"
        , ".tracks { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 18px; padding: 34px clamp(20px, 4vw, 56px) 56px; background: var(--surface); }"
        , ".track-card { min-height: 210px; padding: 24px; border: 1px solid var(--line); border-radius: 8px; background: #ffffff; }"
        , ".metric { display: inline-flex; margin-bottom: 28px; color: var(--accent-strong); font-weight: 800; }"
        , ".track-card p, .review p { color: var(--muted); line-height: 1.65; }"
        , ".review { padding: 56px clamp(20px, 4vw, 56px) 72px; max-width: 920px; }"
        , "textarea { min-height: 150px; resize: vertical; }"
        , ".review textarea { margin: 18px 0 12px; }"
        , "button { color: #ffffff; background: #1f2937; }"
        , ".centered { min-height: 100vh; display: grid; place-items: center; background: var(--surface); }"
        , ".empty-state { text-align: center; padding: 28px; }"
        , ".empty-state h1 { max-width: none; }"
        , "@media (max-width: 820px) { .topbar { align-items: flex-start; flex-direction: column; } .hero { min-height: auto; grid-template-columns: 1fr; } h1 { max-width: 12ch; } .dashboard-grid, .goal-section, .learning-shell, .tracks { grid-template-columns: 1fr; } .learning-top, .practice-toolbar, .practice-footer { flex-direction: column; align-items: stretch; } .learning-stats { grid-template-columns: repeat(2, minmax(0, 1fr)); } .field-grid { grid-template-columns: 1fr; } .question-header { flex-direction: column; } .nav { width: 100%; justify-content: space-between; gap: 10px; overflow-x: auto; } }"
        ]

goalScript :: Text
goalScript =
    Text.unlines
        [ "const form = document.getElementById('goal-form');"
        , "const clearButton = document.getElementById('goal-clear');"
        , "const summary = document.getElementById('goal-summary-text');"
        , "function readGoal() {"
        , "  return {"
        , "    role: document.getElementById('goal-role').value.trim(),"
        , "    industry: document.getElementById('goal-industry').value.trim(),"
        , "    mode: document.getElementById('goal-mode').value,"
        , "    deadline: document.getElementById('goal-deadline').value,"
        , "    success: document.getElementById('goal-success').value.trim()"
        , "  };"
        , "}"
        , "function writeGoal(goal) {"
        , "  document.getElementById('goal-role').value = goal.role || '';"
        , "  document.getElementById('goal-industry').value = goal.industry || '';"
        , "  document.getElementById('goal-mode').value = goal.mode || 'Remoto';"
        , "  document.getElementById('goal-deadline').value = goal.deadline || '30 dias';"
        , "  document.getElementById('goal-success').value = goal.success || '';"
        , "}"
        , "function updateSummary(goal) {"
        , "  if (!goal.role && !goal.industry && !goal.success) {"
        , "    summary.textContent = 'Aun no has definido un objetivo laboral.';"
        , "    return;"
        , "  }"
        , "  const role = goal.role || 'Rol por definir';"
        , "  const industry = goal.industry || 'industria por definir';"
        , "  const success = goal.success ? ' Exito: ' + goal.success : '';"
        , "  summary.textContent = role + ' en ' + industry + ', modalidad ' + goal.mode + ', plazo ' + goal.deadline + '.' + success;"
        , "}"
        , "async function loadGoal() {"
        , "  const response = await fetch('/api/goal');"
        , "  if (!response.ok) return;"
        , "  const payload = await response.json();"
        , "  const goal = payload.goal || {};"
        , "  writeGoal(goal);"
        , "  updateSummary(readGoal());"
        , "}"
        , "form.addEventListener('submit', async (event) => {"
        , "  event.preventDefault();"
        , "  const goal = readGoal();"
        , "  const response = await fetch('/api/goal', {"
        , "    method: 'POST',"
        , "    headers: { 'Content-Type': 'application/json' },"
        , "    body: JSON.stringify(goal)"
        , "  });"
        , "  if (!response.ok) return;"
        , "  updateSummary(goal);"
        , "});"
        , "clearButton.addEventListener('click', async () => {"
        , "  const response = await fetch('/api/goal', { method: 'DELETE' });"
        , "  if (!response.ok) return;"
        , "  writeGoal({});"
        , "  updateSummary(readGoal());"
        , "});"
        , "loadGoal();"
        ]

learningScript :: Text
learningScript =
    Text.unlines
        [ "const topicForm = document.getElementById('topic-form');"
        , "const topicList = document.getElementById('topic-list');"
        , "const learningEmpty = document.getElementById('learning-empty');"
        , "const questionPanel = document.getElementById('question-panel');"
        , "const activeTopicTitle = document.getElementById('active-topic-title');"
        , "const activeTopicLevel = document.getElementById('active-topic-level');"
        , "const activeTopicKnowledge = document.getElementById('active-topic-knowledge');"
        , "const activeTopicProgress = document.getElementById('active-topic-progress');"
        , "const questionText = document.getElementById('question-text');"
        , "const answerOptions = document.getElementById('answer-options');"
        , "const answerFeedback = document.getElementById('answer-feedback');"
        , "const generateQuestionButton = document.getElementById('generate-question');"
        , "const quickGenerateButton = document.getElementById('quick-generate');"
        , "const practiceStateText = document.getElementById('practice-state-text');"
        , "const statTopics = document.getElementById('stat-topics');"
        , "const statKnowledge = document.getElementById('stat-knowledge');"
        , "const statAnswers = document.getElementById('stat-answers');"
        , "const statLevel = document.getElementById('stat-level');"
        , "let topics = [];"
        , "let activeTopic = null;"
        , "let activeQuestion = null;"
        , "async function fetchJson(url, options = {}) {"
        , "  const response = await fetch(url, options);"
        , "  if (!response.ok) {"
        , "    const payload = await response.json().catch(() => ({}));"
        , "    throw new Error(payload.error || 'Solicitud fallida');"
        , "  }"
        , "  if (response.status === 204) return null;"
        , "  return response.json();"
        , "}"
        , "function renderTopics() {"
        , "  topicList.innerHTML = '';"
        , "  if (!topics.length) {"
        , "    topicList.innerHTML = '<p class=\"feedback\">Aun no hay temas.</p>';"
        , "    return;"
        , "  }"
        , "  topics.forEach((topic) => {"
        , "    const button = document.createElement('button');"
        , "    button.type = 'button';"
        , "    button.className = 'topic-item' + (activeTopic && activeTopic.id === topic.id ? ' active' : '');"
        , "    button.innerHTML = `<strong>${escapeHtml(topic.title)}</strong><div class=\"topic-meta\"><span>${topic.knowledge}% conocimiento</span><span>Nivel ${topic.level}</span></div><div class=\"topic-progress\"><span style=\"width: ${topic.knowledge}%\"></span></div><div class=\"topic-meta\"><span>${topic.correct}/${topic.total} correctas</span><span>${escapeHtml(topic.description || 'Sin enfoque')}</span></div>`;"
        , "    button.addEventListener('click', () => selectTopic(topic));"
        , "    topicList.appendChild(button);"
        , "  });"
        , "}"
        , "function updateStats() {"
        , "  const totalTopics = topics.length;"
        , "  const totalAnswers = topics.reduce((sum, topic) => sum + topic.total, 0);"
        , "  const averageKnowledge = totalTopics ? Math.round(topics.reduce((sum, topic) => sum + topic.knowledge, 0) / totalTopics) : 0;"
        , "  statTopics.textContent = totalTopics;"
        , "  statKnowledge.textContent = averageKnowledge + '%';"
        , "  statAnswers.textContent = totalAnswers;"
        , "  statLevel.textContent = activeTopic ? 'Nivel ' + activeTopic.level : '-';"
        , "  quickGenerateButton.disabled = !activeTopic;"
        , "}"
        , "function selectTopic(topic) {"
        , "  activeTopic = topic;"
        , "  activeQuestion = null;"
        , "  learningEmpty.classList.add('hidden');"
        , "  questionPanel.classList.remove('hidden');"
        , "  activeTopicTitle.textContent = topic.title;"
        , "  activeTopicLevel.textContent = 'Nivel ' + topic.level;"
        , "  activeTopicKnowledge.textContent = topic.knowledge + '%';"
        , "  activeTopicProgress.textContent = topic.total ? `${topic.correct} de ${topic.total} respuestas correctas.` : 'Sin respuestas registradas todavía.';"
        , "  practiceStateText.textContent = 'Tema seleccionado';"
        , "  questionText.textContent = 'Genera una pregunta para comenzar la práctica de este tema.';"
        , "  answerOptions.innerHTML = '';"
        , "  answerFeedback.className = 'feedback';"
        , "  answerFeedback.textContent = 'La dificultad se ajustará con base en tus respuestas.';"
        , "  renderTopics();"
        , "  updateStats();"
        , "}"
        , "async function loadTopics() {"
        , "  const payload = await fetchJson('/api/learning/topics');"
        , "  topics = payload.topics;"
        , "  if (activeTopic) activeTopic = topics.find((topic) => topic.id === activeTopic.id) || null;"
        , "  if (activeTopic) selectTopic(activeTopic);"
        , "  renderTopics();"
        , "  updateStats();"
        , "}"
        , "async function generateQuestion() {"
        , "  if (!activeTopic) return;"
        , "  practiceStateText.textContent = 'Generando pregunta con IA';"
        , "  answerFeedback.className = 'feedback';"
        , "  answerFeedback.textContent = 'Preparando una pregunta para tu nivel actual...';"
        , "  answerOptions.innerHTML = '';"
        , "  generateQuestionButton.disabled = true;"
        , "  quickGenerateButton.disabled = true;"
        , "  const payload = await fetchJson(`/api/learning/topics/${activeTopic.id}/question`, { method: 'POST' });"
        , "  activeQuestion = payload.question;"
        , "  questionText.textContent = activeQuestion.question;"
        , "  practiceStateText.textContent = 'Pregunta lista';"
        , "  answerFeedback.textContent = '';"
        , "  activeQuestion.options.forEach((option, index) => {"
        , "    const button = document.createElement('button');"
        , "    button.type = 'button';"
        , "    button.className = 'answer-option';"
        , "    button.textContent = option;"
        , "    button.addEventListener('click', () => submitAnswer(index));"
        , "    answerOptions.appendChild(button);"
        , "  });"
        , "  generateQuestionButton.disabled = false;"
        , "  quickGenerateButton.disabled = false;"
        , "}"
        , "async function submitAnswer(index) {"
        , "  if (!activeQuestion) return;"
        , "  Array.from(answerOptions.children).forEach((button) => { button.disabled = true; });"
        , "  const payload = await fetchJson(`/api/learning/questions/${activeQuestion.id}/answer`, {"
        , "    method: 'POST',"
        , "    headers: { 'Content-Type': 'application/json' },"
        , "    body: JSON.stringify({ selectedIndex: index })"
        , "  });"
        , "  activeTopic = payload.topic;"
        , "  Array.from(answerOptions.children).forEach((button, optionIndex) => {"
        , "    if (optionIndex === payload.correctIndex) button.classList.add('correct');"
        , "    if (optionIndex === index && !payload.correct) button.classList.add('incorrect');"
        , "  });"
        , "  practiceStateText.textContent = payload.correct ? 'Respuesta correcta' : 'Respuesta por reforzar';"
        , "  answerFeedback.className = payload.correct ? 'feedback success' : 'feedback error';"
        , "  answerFeedback.textContent = (payload.correct ? 'Correcto. ' : 'Incorrecto. ') + payload.explanation;"
        , "  activeTopicLevel.textContent = 'Nivel ' + activeTopic.level;"
        , "  activeTopicKnowledge.textContent = activeTopic.knowledge + '%';"
        , "  activeTopicProgress.textContent = `${activeTopic.correct} de ${activeTopic.total} respuestas correctas.`;"
        , "  await loadTopics();"
        , "}"
        , "function escapeHtml(value) {"
        , "  return String(value).replace(/[&<>'\"]/g, (char) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', \"'\": '&#039;', '\"': '&quot;' }[char]));"
        , "}"
        , "topicForm.addEventListener('submit', async (event) => {"
        , "  event.preventDefault();"
        , "  const title = document.getElementById('topic-title').value.trim();"
        , "  const description = document.getElementById('topic-description').value.trim();"
        , "  if (!title) return;"
        , "  const payload = await fetchJson('/api/learning/topics', {"
        , "    method: 'POST',"
        , "    headers: { 'Content-Type': 'application/json' },"
        , "    body: JSON.stringify({ title, description })"
        , "  });"
        , "  topicForm.reset();"
        , "  topics = [payload.topic, ...topics];"
        , "  selectTopic(payload.topic);"
        , "});"
        , "generateQuestionButton.addEventListener('click', generateQuestion);"
        , "quickGenerateButton.addEventListener('click', generateQuestion);"
        , "loadTopics();"
        ]
