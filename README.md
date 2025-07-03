# Loops SDK for Haskell

A self-contained Haskell module that acts as a Loops.so SDK, providing the same interfaces as the existing Loops SDKs for sending transactional emails.

## Features

- **Complete API Coverage**: Supports all transactional email types (invite, validation, password reset)
- **Type Safety**: Uses Python dataclasses and type hints for better development experience
- **Error Handling**: Comprehensive error handling with meaningful error messages
- **Testing**: Extensive test suite with mocking for external API calls
- **Easy Integration**: Simple, intuitive API that mirrors other Loops SDK

## Installation

Add `loops-sdk` to your project. `pip install loops-sdk`

### Dependencies

The SDK requires the following Python packages:
- `requests` (for HTTP API calls)


## Quick Start

```haskell
import LoopsClient
import LoopsEmail
import Attachment

-- Initialize the client
client = LoopsClient(api_key="your-loops-api-key")

# Or use environment variable LOOPS_TOKEN
client = LoopsClient()

# Send an invite email
response = send_transactional_email(
    client=client,
    email="newuser@example.com",
    transactional_id="your-custom-template-id",
    add_to_audience=True,
    data_variables={
        "customerName": "Alice Johnson",
        "invoiceNumber": "INV-001",
        "amount": "$99.99"
    },
    attachments=[attachment]
)

print(f"Email sent successfully: {response}")
```

## API Reference

### LoopsClient

The main client class for interacting with the Loops API.

```haskell
client = LoopsClient(api_key="your-api-key")
```

**Parameters:**
- `api_key` (optional): Your Loops API key. If not provided, will use the `LOOPS_TOKEN` environment variable.

### Email Constructor Functions

#### mk_invite_email(email, inviter_name, invite_url)

Creates an invite email object.

**Parameters:**
- `email`: Recipient's email address
- `inviter_name`: Name of the person sending the invite
- `invite_url`: URL for the invitation

**Returns:** `LoopsEmail` object

#### mk_validation_email(email, name, verification_url)

Creates a validation email object.

**Parameters:**
- `email`: Recipient's email address
- `name`: Recipient's name
- `verification_url`: URL for email verification

**Returns:** `LoopsEmail` object

#### mk_password_reset_email(email, name, reset_url)

Creates a password reset email object.

**Parameters:**
- `email`: Recipient's email address
- `name`: Recipient's name
- `reset_url`: URL for password reset

**Returns:** `LoopsEmail` object


## Usage Examples

### Basic Usage

```haskell
import LoopsClient
import LoopsEmail
import Attachment

-- Initialize client
client = LoopsClient(api_key="your-api-key")

# Create and send an invite email
invite_email = mk_invite_email(
    email="user@example.com",
    inviter_name="John Doe",
    invite_url="https://example.com/invite/123"
)

response = client.send_transactional_email(invite_email)
```

### Using Environment Variables

```haskell
import os
from loops_sdk import LoopsClient, send_validation_email

# Set environment variable
os.environ["LOOPS_TOKEN"] = "your-api-key"

# Client will automatically use the environment variable
client = LoopsClient()

# Send validation email
response = send_validation_email(
    client=client,
    email="user@example.com",
    name="Jane Smith",
    verification_url="https://example.com/verify/abc123"
)
```

### Error Handling

```haskell
import requests
from loops_sdk import LoopsClient, send_password_reset_email

client = LoopsClient(api_key="your-api-key")

try:
    response = send_password_reset_email(
        client=client,
        email="user@example.com",
        name="Bob Johnson",
        reset_url="https://example.com/reset/xyz789"
    )
    print("Email sent successfully!")
except requests.HTTPError as e:
    print(f"Failed to send email: {e}")
except ValueError as e:
    print(f"Configuration error: {e}")
```

### Custom Email with Attachments

```haskell
from loops_sdk import LoopsClient, LoopsEmail, Attachment

client = LoopsClient(api_key="your-api-key")

# Create an attachment
attachment = Attachment(
    filename="invoice.pdf",
    content_type="application/pdf",
    data="base64-encoded-data-here"
)

# Create custom email
custom_email = LoopsEmail(
    email="customer@example.com",
    transactional_id="your-custom-template-id",
    add_to_audience=True,
    data_variables={
        "customerName": "Alice Johnson",
        "invoiceNumber": "INV-001",
        "amount": "$99.99"
    },
    attachments=[attachment]
)

response = client.send_transactional_email(custom_email)
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