# Career Trainer

Career Trainer is a Haskell web application for structured career preparation. It helps a user define a target role, organize learning topics, practice with AI-generated questions, and track topic-level knowledge over time.

The project is built as a portfolio-ready application: reproducible development environment, server-rendered UI, SQLite persistence, and OpenAI integration.

## Features

- Career goal page with persistent SQLite storage.
- Learning workspace for adding technical or professional topics.
- AI-generated multiple-choice questions using OpenAI.
- Adaptive topic level from 1 to 5 based on answer history.
- Per-topic knowledge percentage and answer counts.
- Multipage application structure: dashboard, goal, and learning.
- Nix flake for development, build, and runtime tooling.

## Tech Stack

- Haskell with GHC 9.12.3
- Scotty 0.30 over WAI/Warp
- Lucid2 for type-safe HTML
- SQLite through sqlite-simple
- OpenAI Responses API with Structured Outputs
- Nix flakes for reproducible environments

## Getting Started

### Requirements

- Nix 2.31 or newer with flakes enabled.

### Configure OpenAI

Create a local `.env` file from the example:

```sh
cp .env.example .env
```

Then set your API key:

```sh
OPENAI_API_KEY=your_api_key_here
OPENAI_MODEL=gpt-5.6
```

`.env` is ignored by Git. Do not commit real API keys.

### Run Locally

```sh
nix run
```

The application runs at:

```text
http://localhost:3000
```

For an interactive development shell:

```sh
nix develop
cabal run career-trainer
```

## Routes

- `/` - Main dashboard.
- `/objetivo` - Career goal editor.
- `/aprendizaje` - Learning topics and adaptive practice.
- `/health` - Service healthcheck.
- `/api/goal` - Read, save, and delete the career goal.
- `/api/learning/topics` - List and create learning topics.
- `/api/learning/topics/:id/question` - Generate a topic question with OpenAI.
- `/api/learning/questions/:id/answer` - Save an answer and update progress.

## Database

The app uses SQLite and creates `career-trainer.sqlite3` automatically in the directory where the server runs.

Generated database files are ignored by Git:

- `*.sqlite3`
- `*.sqlite3-shm`
- `*.sqlite3-wal`

## Development Commands

```sh
nix build
nix flake check
nix fmt
ghcid --command "cabal repl career-trainer"
```

## Architecture Notes

The current implementation keeps the app in a compact Haskell executable. The backend owns persistence and API behavior, while the frontend uses server-rendered HTML plus small JavaScript controllers for form submission and learning interactions.

OpenAI responses are requested as structured JSON so generated questions can be parsed reliably and stored in SQLite.

## Security Notes

- Never commit `.env` or real API keys.
- Rotate any API key that is accidentally printed in terminal output or committed.
- Billing and quota errors from OpenAI are surfaced through fallback questions so the UI remains usable during local development.

## License

MIT
