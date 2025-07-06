# Loops SDK for Haskell

A self-contained Haskell module that acts as a Loops.so SDK, providing the same interfaces as the existing Loops SDKs for sending transactional emails.

## Features

- **Complete API Coverage**: Supports all transactional email types (invite, validation, password reset)
- **Type Safety**: Uses Python dataclasses and type hints for better development experience
- **Error Handling**: Comprehensive error handling with meaningful error messages
- **Testing**: Extensive test suite with mocking for external API calls
- **Easy Integration**: Simple, intuitive API that mirrors other Loops SDK

## Installation

Add `loops` to your project.


## Quick Start

```haskell
{-# LANGUAGE OverloadedStrings #-}

import LoopsSDK
import Data.Aeson (object, (.=))
import Data.Text (Text)

main :: IO ()
main = do
    -- Initialise the client (optionally pass Nothing for the default API root)
    let client = LoopsClient "your-loops-api-key"

    -- Build the email payload
    let email =
            LoopsEmail
                { leEmail = "newuser@example.com"
                , leTransactionalId = "your-custom-template-id"
                , leAddToAudience = Just True
                , leDataVariables = Just $ object
                    [ "customerName"  .= ("Alice Johnson" :: Text)
                    , "invoiceNumber" .= ("INV-001"       :: Text)
                    , "amount"        .= ("$99.99"        :: Text)
                    ]
                , leAttachments = []
                }

    -- Send the email
    resp <- sendTransactionalEmail client email Nothing
    print resp
```

## API Reference

### LoopsClient

The main client for interacting with the Loops API.

```haskell
let client = LoopsClient "your-api-key"
```

**Parameters:**
- First argument: Your Loops API key (required)

**Available Functions:**
- `testApiKey` - Test your API key
- `createContact` - Create a new contact
- `updateContact` - Update an existing contact
- `findContact` - Find a contact by email or user ID
- `deleteContact` - Delete a contact
- `createContactProperty` - Create a custom contact property
- `getContactProperties` - Get contact properties
- `getMailingLists` - Get all mailing lists
- `sendEvent` - Send an event
- `sendTransactionalEmail` - Send a transactional email
- `getTransactionalEmails` - Get sent transactional emails


## Usage Examples

### Basic Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import LoopsSDK
import Data.Aeson (object, (.=))
import Data.Text (Text)

main :: IO ()
main = do
    let client = LoopsClient "your-api-key"

    let inviteEmail =
            LoopsEmail
                { leEmail = "user@example.com"
                , leTransactionalId = "your-invite-template-id"
                , leAddToAudience = Just True
                , leDataVariables = Just $ object
                    [ "inviterName" .= ("John Doe" :: Text)
                    , "inviteUrl"   .= ("https://example.com/invite/123" :: Text)
                    ]
                , leAttachments = []
                }

    _ <- sendTransactionalEmail client inviteEmail Nothing
    pure ()
```

### Using Environment Variables

```haskell
{-# LANGUAGE OverloadedStrings #-}

import LoopsSDK
import System.Environment (lookupEnv)
import Data.Text (pack)

main :: IO ()
main = do
    token <- fmap pack <$> lookupEnv "LOOPS_TOKEN" >>= maybe (fail "LOOPS_TOKEN not set") pure
    let client = LoopsClient token
    putStrLn "Client initialised successfully – ready to send emails!"
```

### Error Handling

```haskell
{-# LANGUAGE OverloadedStrings #-}

import LoopsSDK
import Control.Exception (try)
import Data.Aeson (Value)

main :: IO ()
main = do
    let client = LoopsClient "your-api-key"
    result <- try $ do
        let email = LoopsEmail "user@example.com" "reset-template-id" (Just True) Nothing []
        sendTransactionalEmail client email Nothing :: IO Value
    case result of
        Left (e :: APIError) -> putStrLn $ "Failed to send email: " <> show e
        Right _             -> putStrLn "Email sent successfully!"
```

### Custom Email with Attachments

```haskell
{-# LANGUAGE OverloadedStrings #-}

import LoopsSDK
import Data.Aeson (object, (.=))
import Data.Text (Text)

main :: IO ()
main = do
    let client = LoopsClient "your-api-key"

    let attachment = Attachment
            { filename = "invoice.pdf"
            , contentType = "application/pdf"
            , data_ = "base64-encoded-data-here"
            }

        customEmail = LoopsEmail
            { leEmail = "customer@example.com"
            , leTransactionalId = "your-custom-template-id"
            , leAddToAudience = Just True
            , leDataVariables = Just $ object
                [ "customerName"  .= ("Alice Johnson" :: Text)
                , "invoiceNumber" .= ("INV-001"       :: Text)
                , "amount"        .= ("$99.99"        :: Text)
                ]
            , leAttachments = [attachment]
            }

    _ <- sendTransactionalEmail client customEmail Nothing
    putStrLn "Email sent!"
```

## Testing

The SDK includes a comprehensive test suite. To run the tests:

```bash
make test
```

### Test Coverage

The test suite covers:
- Email object creation and serialization
- API client initialization and configuration
- Successful API calls with mocked responses
- Error handling for failed API calls
- All convenience functions
- Integration workflows
- Edge cases and error conditions

### Running Tests with Coverage

```bash
make test
```

## Compatibility

This SDK is designed to be compatible with the existing Loops SDKs and follows the same patterns:

- **Haskell SDK**: Mirrors the data structures and function names
- **JavaScript SDK**: Similar API design and error handling
- **PHP SDK**: Comparable class structure and method signatures

## Error Handling

The SDK provides comprehensive error handling:

- **ValueError**: Raised when API key is missing or invalid configuration
- **requests.HTTPError**: Raised when API calls fail with detailed error messages
- **JSON Errors**: Handled gracefully with fallback to empty responses

## Environment Variables

- `LOOPS_TOKEN`: Your Loops API key (alternative to passing it directly)

## Contributing

This is a self-contained module designed to be copied into projects. If you need to modify it:

1. Update the `src/loops_sdk.hs` file
2. Add corresponding tests in `test/test_loops_sdk.hs`
3. Update this README with any new features or changes

## License

This SDK is provided as-is for integration with Loops.so. Please refer to Loops.so's terms of service for API usage guidelines.