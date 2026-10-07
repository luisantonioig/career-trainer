{-# LANGUAGE OverloadedStrings #-}

module Generation (main) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM_, unless)
import Data.Aeson (Value (..), eitherDecode, encode, object, toJSON, (.=))
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite.Simple
import Main qualified as App
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (ConnectionTimeout))
import Network.HTTP.Types (Status, methodPost, status200, status404, status409, status503)
import Network.Wai (Application, requestMethod, requestHeaders)
import Network.Wai.Test
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile, withCurrentDirectory)
import System.Environment (setEnv, unsetEnv)
import System.IO (hClose, openTempFile)
import Web.Scotty (scottyApp)

main :: IO ()
main = do
  tmp <- getTemporaryDirectory
  bracket (newDirectory tmp) removeDirectoryRecursive $ \dir -> withCurrentDirectory dir $ do
    App.initializeDatabase
    let questionFixture = object
          [ "question" .= ("What does STM provide?" :: Text)
          , "options" .= (["Atomic transactions", "HTTP", "CSS", "Garbage collection"] :: [Text])
          , "correctIndex" .= (0 :: Int)
          , "explanation" .= ("STM composes atomic memory transactions." :: Text)
          ]
        response question = encode (object ["output_text" .= question])
        questionResponse = response . Text.decodeUtf8 . LBS.toStrict . encode
        validBody = questionResponse questionFixture
        invalidBodies = case questionFixture of
          Object fields -> map (questionResponse . Object . (\(key, value) -> KeyMap.insert key value fields))
            [ ("question", String " ")
            , ("options", toJSON (["a"] :: [Text]))
            , ("options", toJSON (["a", " ", "c", "d"] :: [Text]))
            , ("correctIndex", toJSON (-1 :: Int))
            , ("correctIndex", toJSON (4 :: Int))
            , ("explanation", String " ")
            ]
          _ -> error "question fixture must be an object"
        generator transport = App.requestOpenAIQuestionWith transport
    -- Real production generation with absent/empty keys must never reach transport.
    forM_ [Nothing, Just "", Just "   "] $ \key -> do
      maybe (unsetEnv "OPENAI_API_KEY") (setEnv "OPENAI_API_KEY") key
      app <- scottyApp (App.applicationRoutes (generator (\_ -> error "transport called without API key")))
      checkFailure app status503
    setEnv "OPENAI_API_KEY" "test-only-key"
    -- Only the external transport is stubbed; parsing, routes and SQLite are real.
    forM_ ([\req -> throwIO (HttpExceptionRequest req ConnectionTimeout),
            \_ -> pure (status503, validBody),
            \_ -> pure (status200, "not JSON"),
            \_ -> pure (status200, "{}"),
            \_ -> pure (status200, response ("not a question" :: Text))]
           <> map (\invalidBody _ -> pure (status200, invalidBody)) invalidBodies) $ \transport -> do
      app <- scottyApp (App.applicationRoutes (generator transport))
      checkFailure app status503
    app <- scottyApp (App.applicationRoutes (generator (\_ -> pure (status200, validBody))))
    missing <- post app "/api/learning/topics/2147483647/question"
    assert "missing topic returns 404" (simpleStatus missing == status404)
    topicId <- createTopic
    before <- topicRows topicId
    success <- post app (topicPath topicId)
    assert "valid generated question succeeds" (simpleStatus success == status200)
    after <- topicRows topicId
    assert "generation alone preserves progress" (before == after)
    rows <- withConnection "career-trainer.sqlite3" $ \conn -> query conn "SELECT question, options_json, correct_index, explanation FROM learning_questions WHERE topic_id = ?" (Only topicId) :: IO [(Text, Text, Int, Text)]
    assert "valid question is persisted exactly once" (length rows == 1)
    -- Old development fallbacks already stored by earlier versions are not assessments.
    forM_ ["Sin OPENAI_API_KEY configurada. Pregunta de prueba para el tema: Haskell STM.", "OPENAI_API_KEY fue detectada, pero no se pudo obtener una pregunta de OpenAI para Haskell STM."] $ \legacy -> do
      questionId <- withConnection "career-trainer.sqlite3" $ \conn -> do
        execute conn "INSERT INTO learning_questions (topic_id, question, options_json, correct_index, explanation, difficulty) VALUES (?, ?, ?, ?, ?, ?)" (topicId, legacy :: Text, "[\"a\",\"b\",\"c\",\"d\"]" :: Text, 0 :: Int, "Legacy diagnostic" :: Text, 1 :: Int)
        lastInsertRowId conn
      answered <- postBody app ("/api/learning/questions/" <> show questionId <> "/answer") (encode (object ["selectedIndex" .= (0 :: Int)]))
      assert "legacy fallback cannot be scored" (simpleStatus answered == status409)
      final <- topicRows topicId
      assert "legacy fallback preserves progress" (before == final)
  putStrLn "All generation integration tests passed."

newDirectory :: FilePath -> IO FilePath
newDirectory tmp = do
  (path, handle) <- openTempFile tmp "career-trainer-generation"
  hClose handle
  removeFile path
  createDirectory path
  pure path

assert :: String -> Bool -> IO ()
assert label ok = unless ok (ioError (userError label))

createTopic :: IO Int
createTopic = withConnection "career-trainer.sqlite3" $ \conn -> do
  execute_ conn "INSERT INTO learning_topics (title, description, level, knowledge, correct_answers, total_answers) VALUES ('Haskell STM', 'Atomic memory transactions', 3, 50, 2, 4)"
  fromIntegral <$> lastInsertRowId conn

topicRows :: Int -> IO [(Int, Int, Int, Int)]
topicRows topicId = withConnection "career-trainer.sqlite3" $ \conn -> query conn "SELECT level, knowledge, correct_answers, total_answers FROM learning_topics WHERE id = ?" (Only topicId)

topicPath :: Int -> String
topicPath topicId = "/api/learning/topics/" <> show topicId <> "/question"

post :: Application -> String -> IO SResponse
post app path = postBody app path ""

postBody :: Application -> String -> LBS.ByteString -> IO SResponse
postBody app path body = runSession (srequest (SRequest req body)) app
  where
    req = (setPath defaultRequest (BS.pack path))
      { requestMethod = methodPost
      , requestHeaders = [("Content-Type", "application/json")]
      }

checkFailure :: Application -> Status -> IO ()
checkFailure app expectedStatus = do
  topicId <- createTopic
  before <- topicRows topicId
  failed <- post app (topicPath topicId)
  assert "generation failure returns 503" (simpleStatus failed == expectedStatus)
  assert "generation error explains failure" (case eitherDecode (simpleBody failed) :: Either String Value of
    Right (Object obj) -> case KeyMap.lookup "error" obj of
      Just (String message) -> not (Text.null message)
      _ -> False
    _ -> False)
  after <- topicRows topicId
  assert "generation failure preserves every progress field" (before == after)
  count <- withConnection "career-trainer.sqlite3" $ \conn -> query conn "SELECT count(*) FROM learning_questions WHERE topic_id = ?" (Only topicId) :: IO [Only Int]
  assert "generation failure inserts no questions" (count == [Only 0])
