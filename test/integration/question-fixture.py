"""Seed a valid STM assessment in the isolated integration-test database."""
import json
import sqlite3
import sys

connection = sqlite3.connect(sys.argv[1])
topic_id = int(sys.argv[2])
question = "What does Haskell STM provide?"
options = ["Atomic memory transactions", "HTTP routing", "CSS styling", "Garbage collection"]
cursor = connection.execute(
    "INSERT INTO learning_questions "
    "(topic_id, question, options_json, correct_index, explanation, difficulty) "
    "VALUES (?, ?, ?, 0, ?, 1)",
    (topic_id, question, json.dumps(options), "STM composes atomic memory transactions."),
)
connection.commit()
print(json.dumps({"question": {"id": cursor.lastrowid, "topicId": topic_id,
                             "question": question, "options": options, "difficulty": 1}}))
connection.close()
