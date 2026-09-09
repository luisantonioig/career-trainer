#!/usr/bin/env bash

set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

ROOT_DIR="$(
  cd "$(dirname "${BASH_SOURCE[0]}")/../.."
  pwd
)"

BASE_URL="http://127.0.0.1:3000"

cd "$ROOT_DIR"


# ---------------------------------------------------------------------------
# Small test helpers
# ---------------------------------------------------------------------------

fail() {
  echo
  echo "❌ TEST FAILED: $1"

  if [[ -f "${SERVER_LOG:-}" ]]; then
    echo
    echo "--- server log ---"
    cat "$SERVER_LOG"
    echo "------------------"
  fi

  exit 1
}

assert_eq() {
  local expected="$1"
  local actual="$2"
  local description="$3"

  if [[ "$actual" != "$expected" ]]; then
    fail "$description: expected '$expected', got '$actual'"
  fi

  echo "✓ $description"
}

require_command() {
  local command_name="$1"

  if ! command -v "$command_name" >/dev/null 2>&1; then
    fail "required command '$command_name' was not found"
  fi
}


# ---------------------------------------------------------------------------
# Required tooling
# ---------------------------------------------------------------------------

require_command curl
require_command jq


# ---------------------------------------------------------------------------
# Build the real application
# ---------------------------------------------------------------------------

if [[ -n "${APP_BINARY:-}" ]]; then
  # Resolve before changing into the isolated runtime directory.
  APP_BINARY="$(realpath "$APP_BINARY")"
  [[ -x "$APP_BINARY" ]] || fail "APP_BINARY is not executable: $APP_BINARY"
  echo "Using built application: $APP_BINARY"
else
  require_command cabal
  echo
  echo "Building career-trainer..."
  cabal build exe:career-trainer
  APP_BINARY="$(cabal list-bin exe:career-trainer)"
fi


# ---------------------------------------------------------------------------
# Isolated runtime
#
# The application uses a relative database path:
#
#     career-trainer.sqlite3
#
# Running the binary from a temporary directory therefore gives this test
# its own fresh database without touching the developer's real data.
# ---------------------------------------------------------------------------

TEST_DIR="$(mktemp -d)"
SERVER_LOG="$TEST_DIR/server.log"
SERVER_PID=""

cleanup() {
  if [[ -n "$SERVER_PID" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
    kill "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
  fi

  rm -rf "$TEST_DIR"
}

trap cleanup EXIT


# ---------------------------------------------------------------------------
# Start server
#
# OPENAI_API_KEY is intentionally removed. The application already has a
# deterministic fallback question, so this test does not depend on the
# network or an external API.
# ---------------------------------------------------------------------------

echo "Starting isolated server..."

(
  cd "$TEST_DIR"

  exec env -u OPENAI_API_KEY \
    "$APP_BINARY" \
    >"$SERVER_LOG" \
    2>&1
) &

SERVER_PID=$!


# ---------------------------------------------------------------------------
# Wait until the application is actually ready.
# ---------------------------------------------------------------------------

SERVER_READY=false

for _ in $(seq 1 100); do
  if curl --silent --fail "$BASE_URL/health" >/dev/null 2>&1; then
    SERVER_READY=true
    break
  fi

  if ! kill -0 "$SERVER_PID" 2>/dev/null; then
    fail "server terminated before becoming ready"
  fi

  sleep 0.1
done

if [[ "$SERVER_READY" != true ]]; then
  fail "server did not become ready"
fi

echo "✓ server is healthy"


# ---------------------------------------------------------------------------
# Create a fresh learning topic.
# ---------------------------------------------------------------------------

TOPIC_RESPONSE="$(
  curl \
    --silent \
    --show-error \
    --fail \
    --request POST \
    --header 'Content-Type: application/json' \
    --data '{
      "title": "Integration test topic",
      "description": "Temporary topic for answer idempotency tests"
    }' \
    "$BASE_URL/api/learning/topics"
)"

TOPIC_ID="$(jq -er '.topic.id' <<<"$TOPIC_RESPONSE")"

assert_eq "0" "$(jq -er '.topic.total' <<<"$TOPIC_RESPONSE")" \
  "new topic starts with zero answers"


# ---------------------------------------------------------------------------
# Generate one question.
#
# Since OPENAI_API_KEY is absent, this should use the deterministic fallback
# instead of making the test depend on OpenAI availability.
# ---------------------------------------------------------------------------

QUESTION_RESPONSE="$(
  curl \
    --silent \
    --show-error \
    --fail \
    --request POST \
    "$BASE_URL/api/learning/topics/$TOPIC_ID/question"
)"

QUESTION_ID="$(jq -er '.question.id' <<<"$QUESTION_RESPONSE")"
OPTION_COUNT="$(jq -er '.question.options | length' <<<"$QUESTION_RESPONSE")"

echo "✓ generated question $QUESTION_ID with $OPTION_COUNT options"

# ---------------------------------------------------------------------------
# TEST 1
#
# A negative option index is invalid.
#
# Expected:
#   HTTP 400
#   no progress mutation
# ---------------------------------------------------------------------------

BODY_FILE="$TEST_DIR/response.json"

STATUS="$(
  curl \
    --silent \
    --show-error \
    --output "$BODY_FILE" \
    --write-out '%{http_code}' \
    --request POST \
    --header 'Content-Type: application/json' \
    --data '{"selectedIndex":-1}' \
    "$BASE_URL/api/learning/questions/$QUESTION_ID/answer"
)"

assert_eq "400" "$STATUS" \
  "negative selectedIndex is rejected"


TOPIC_AFTER_INVALID="$(
  curl \
    --silent \
    --show-error \
    --fail \
    "$BASE_URL/api/learning/topics/$TOPIC_ID"
)"

assert_eq "0" "$(jq -er '.topic.total' <<<"$TOPIC_AFTER_INVALID")" \
  "invalid answer does not change total answers"


# ---------------------------------------------------------------------------
# TEST 2
#
# selectedIndex == number of options is already outside the valid range.
#
# For N options the valid indices are:
#
#     0 .. N - 1
#
# Expected:
#   HTTP 400
# ---------------------------------------------------------------------------

STATUS="$(
  curl \
    --silent \
    --show-error \
    --output "$BODY_FILE" \
    --write-out '%{http_code}' \
    --request POST \
    --header 'Content-Type: application/json' \
    --data "{\"selectedIndex\":$OPTION_COUNT}" \
    "$BASE_URL/api/learning/questions/$QUESTION_ID/answer"
)"

assert_eq "400" "$STATUS" \
  "selectedIndex equal to option count is rejected"


# ---------------------------------------------------------------------------
# TEST 3
#
# First legitimate answer.
#
# We deliberately choose option 0. It does not matter whether it is correct:
# every valid first answer must increment total_answers exactly once.
#
# Expected:
#   HTTP 200
#   total = 1
# ---------------------------------------------------------------------------

STATUS="$(
  curl \
    --silent \
    --show-error \
    --output "$BODY_FILE" \
    --write-out '%{http_code}' \
    --request POST \
    --header 'Content-Type: application/json' \
    --data '{"selectedIndex":0}' \
    "$BASE_URL/api/learning/questions/$QUESTION_ID/answer"
)"

assert_eq "200" "$STATUS" \
  "first valid answer succeeds"


TOPIC_AFTER_FIRST="$(
  curl \
    --silent \
    --show-error \
    --fail \
    "$BASE_URL/api/learning/topics/$TOPIC_ID"
)"

TOTAL_AFTER_FIRST="$(jq -er '.topic.total' <<<"$TOPIC_AFTER_FIRST")"
CORRECT_AFTER_FIRST="$(jq -er '.topic.correct' <<<"$TOPIC_AFTER_FIRST")"

assert_eq "1" "$TOTAL_AFTER_FIRST" \
  "first answer increments total exactly once"


# ---------------------------------------------------------------------------
# TEST 4 — the important regression test
#
# Send EXACTLY the same answer again.
#
# Expected:
#   HTTP 409
#
# More importantly:
#
#   total_answers stays 1
#   correct_answers stays unchanged
#
# This is the test that would fail with the old implementation.
# ---------------------------------------------------------------------------

STATUS="$(
  curl \
    --silent \
    --show-error \
    --output "$BODY_FILE" \
    --write-out '%{http_code}' \
    --request POST \
    --header 'Content-Type: application/json' \
    --data '{"selectedIndex":0}' \
    "$BASE_URL/api/learning/questions/$QUESTION_ID/answer"
)"

assert_eq "409" "$STATUS" \
  "answering the same question twice returns conflict"


TOPIC_AFTER_DUPLICATE="$(
  curl \
    --silent \
    --show-error \
    --fail \
    "$BASE_URL/api/learning/topics/$TOPIC_ID"
)"

TOTAL_AFTER_DUPLICATE="$(jq -er '.topic.total' <<<"$TOPIC_AFTER_DUPLICATE")"
CORRECT_AFTER_DUPLICATE="$(jq -er '.topic.correct' <<<"$TOPIC_AFTER_DUPLICATE")"

assert_eq "$TOTAL_AFTER_FIRST" "$TOTAL_AFTER_DUPLICATE" \
  "duplicate answer does not increment total"

assert_eq "$CORRECT_AFTER_FIRST" "$CORRECT_AFTER_DUPLICATE" \
  "duplicate answer does not increment correct answers"

# ---------------------------------------------------------------------------
# TEST 5 — another regression test
#
# Send the same answer concurrently.
#
# Expected:
#   HTTP 200
#   HTTP 409
# 
#   or
# 
#   HTTP 409
#   HTTP 200
#
# ---------------------------------------------------------------------------

# Generate new question for the topic

RACE_QUESTION_RESPONSE="$(
  curl \
    --silent \
    --show-error \
    --fail \
    --request POST \
    "$BASE_URL/api/learning/topics/$TOPIC_ID/question"
)"

RACE_QUESTION_ID="$(jq -er '.question.id' <<<"$RACE_QUESTION_RESPONSE")"

curl \
  --silent \
  --show-error \
  --output "$TEST_DIR/body-a.json" \
  --write-out '%{http_code}' \
  --request POST \
  --header 'Content-Type: application/json' \
  --data '{"selectedIndex": 0}' \
  "$BASE_URL/api/learning/questions/$RACE_QUESTION_ID/answer" \
  > "$TEST_DIR/status-a" &

PID_A=$!

curl \
  --silent \
  --show-error \
  --output "$TEST_DIR/body-b.json" \
  --write-out '%{http_code}' \
  --request POST \
  --header 'Content-Type: application/json' \
  --data '{"selectedIndex": 0}' \
  "$BASE_URL/api/learning/questions/$RACE_QUESTION_ID/answer" \
  > "$TEST_DIR/status-b" &

PID_B=$!

wait "$PID_A"
wait "$PID_B"

STATUS_A=$(cat "$TEST_DIR/status-a")
STATUS_B=$(cat "$TEST_DIR/status-b")

if [[ "$STATUS_A" == "200" && "$STATUS_B" == "409" ]] ||
     [[ "$STATUS_A" == "409" && "$STATUS_B" == "200" ]]; then
  :
else
  fail "concurrent answers: expected one 200 and one 409, got $STATUS_A and $STATUS_B"
fi

TOPIC_AFTER_RACE="$(
  curl \
    --silent \
    --show-error \
    --fail \
    "$BASE_URL/api/learning/topics/$TOPIC_ID"
)"

TOTAL_AFTER_RACE="$(jq -er '.topic.total' <<<"$TOPIC_AFTER_RACE")"

EXPECTED_TOTAL_AFTER_RACE=$((TOTAL_AFTER_DUPLICATE + 1))

assert_eq "$EXPECTED_TOTAL_AFTER_RACE" "$TOTAL_AFTER_RACE" \
  "concurrent answers increment total exactly once"

# ---------------------------------------------------------------------------
# TEST 6
#
# Unknown question.
#
# Expected:
#   HTTP 404
# ---------------------------------------------------------------------------

STATUS="$(
  curl \
    --silent \
    --show-error \
    --output "$BODY_FILE" \
    --write-out '%{http_code}' \
    --request POST \
    --header 'Content-Type: application/json' \
    --data '{"selectedIndex":0}' \
    "$BASE_URL/api/learning/questions/2147483647/answer"
)"

assert_eq "404" "$STATUS" \
  "unknown question returns not found"


# ---------------------------------------------------------------------------
# Result
# ---------------------------------------------------------------------------

echo
echo "✅ All answer-question integration tests passed."
