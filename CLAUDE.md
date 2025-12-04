# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell SDK for Loops.so, an email platform. It provides a synchronous HTTP client for the Loops REST API using `http-conduit` and `aeson` for JSON serialization.

## Build Commands

```bash
# Initial setup (installs pre-commit hooks if available, builds project)
make setup

# Build the project
stack build

# Run tests
make test
# or: stack test

# Lint with hlint
make lint

# Format code with fourmolu
make format
```

## Architecture

The SDK is split into two modules:

- **`src/Loops.hs`** - Public API module exposing all client functions (contacts, events, transactional emails, mailing lists)
- **`src/Internal/Loops.hs`** - Internal implementation containing HTTP helpers, request building, error types, and data models

### Key Types

- `LoopsClient` - Client initialized with API key
- `LoopsEmail a` - Generic transactional email payload (parameterized by data variables type)
- `Attachment` - Email attachment with filename, content type, and base64 data
- Error types: `APIError`, `RateLimitExceededError`, `ValidationError`

### API Functions

All functions take a `LoopsClient` as the first argument and return `IO Value`:
- Contact management: `createContact`, `updateContact`, `findContact`, `deleteContact`
- Properties/lists: `createContactProperty`, `getContactProperties`, `getMailingLists`
- Messaging: `sendEvent`, `sendTransactionalEmail`, `getTransactionalEmails`
- Utility: `testApiKey`

### Conventions

- Field prefixes on record types are stripped during JSON serialization (e.g., `cpFirstName` becomes `firstName`)
- Email validation uses regex before API calls
- Functions throw exceptions for validation errors and API failures
