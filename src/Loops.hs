{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Loops
Description : Haskell SDK for Loops (https://loops.so)

This implementation mirrors the public interface of the official TypeScript SDK
and the Python reference implementation bundled with this repository.
All endpoints, error-handling behaviour, and payload shapes follow the public
REST documentation.

The module provides a lightweight synchronous client implemented with
@http-conduit@ (via @http-simple@) and uses @aeson@ for JSON serialisation.
-}
module Loops (
    -- * Core client & errors
    LoopsClient (..),
    Attachment (..),
    LoopsEmail (..),
    RateLimitExceededError (..),
    APIError (..),
    ValidationError (..),

    -- * Contacts
    testApiKey,
    createContact,
    updateContact,
    findContact,
    deleteContact,
    ContactProperties (..),

    -- * Contact properties & mailing lists
    createContactProperty,
    getContactProperties,
    getMailingLists,
    MailingList (..),

    -- * Events & transactional emails
    sendEvent,
    buildSendEventPayload,
    sendTransactionalEmail,
    getTransactionalEmails,
    EventProperties (..),

    -- * Webhook event parsing
    parseWebhookEvent,
    parseWebhookEventValue,
    WebhookEvent (..),
    WebhookPayload (..),
    ContactIdentity (..),
    WebhookEmailRef (..),
    WebhookMailingList (..),
)
where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.Aeson (
    Object,
    ToJSON (toEncoding, toJSON),
    Value (..),
    defaultOptions,
    fieldLabelModifier,
    genericToEncoding,
    genericToJSON,
    object,
    (.=),
 )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Internal.Loops

{- | Contact properties to update when sending an event.

Loops' Send Event API expects contact properties as top-level attributes
in the request body, alongside @email@ and @eventName@ (see
<https://loops.so/docs/api-reference/send-event>). 'sendEvent' flattens
the wrapped 'Object' to top-level keys for you. Loops recognises the
standard property names (@firstName@, @lastName@, @subscribed@,
@userGroup@, @source@) and updates the contact's default fields; any
other key is stored as a custom contact property.

Example:

@
ContactProperties $ KM.fromList
  [ (\"subscribed\", Bool True)
  , (\"userGroup\", String \"Administrator\")
  , (\"organizationKind\", String \"PublisherOrg\")
  ]
@
-}
newtype ContactProperties = ContactProperties Object
    deriving (Show, Eq)

instance ToJSON ContactProperties where
    toJSON (ContactProperties o) = Object o

-- | Represents event-specific properties
data EventProperties = EventProperties
    { epValue :: Maybe Double
    , epCategory :: Maybe Text
    , epUrl :: Maybe Text
    -- Add more event properties as needed
    }
    deriving (Show, Eq, Generic)

instance ToJSON EventProperties where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = drop 2 -- Remove 'ep' prefix
                }
    toEncoding =
        genericToEncoding
            defaultOptions
                { fieldLabelModifier = drop 2 -- Remove 'ep' prefix
                }

-- | Represents a mailing list subscription
data MailingList = MailingList
    { mlId :: Text
    , mlName :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON MailingList where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = drop 2 -- Remove 'ml' prefix
                }
    toEncoding =
        genericToEncoding
            defaultOptions
                { fieldLabelModifier = drop 2 -- Remove 'ml' prefix
                }

-- ---------------------------------------------------------------------------
-- Public API – Contacts
-- ---------------------------------------------------------------------------

testApiKey :: LoopsClient -> IO Value
testApiKey client = perform (baseRequest client "v1/api-key")

createContact :: LoopsClient -> Text -> Maybe Object -> Maybe Object -> IO Value
createContact client email props mailingLists = do
    validateEmail email
    let payload =
            object
                ( ["email" .= email]
                    ++ maybe [] objectToPairs props
                    ++ maybe [] ((: []) . ("mailingLists" .=)) mailingLists
                )
    postJson client "v1/contacts/create" payload

updateContact :: LoopsClient -> Text -> Object -> Maybe Object -> IO Value
updateContact client email props mailingLists = do
    validateEmail email
    let payload =
            object
                ( ["email" .= email]
                    ++ objectToPairs props
                    ++ maybe [] ((: []) . ("mailingLists" .=)) mailingLists
                )
    putJson client "v1/contacts/update" payload

findContact :: LoopsClient -> Either Text Text -> IO Value
findContact client identifier = case identifier of
    Left email -> do
        validateEmail email
        get client "v1/contacts/find" [("email", Just $ BS8.pack $ T.unpack email)]
    Right userId ->
        get client "v1/contacts/find" [("userId", Just $ BS8.pack $ T.unpack userId)]

deleteContact :: LoopsClient -> Either Text Text -> IO Value
deleteContact client identifier = case identifier of
    Left email -> do
        validateEmail email
        postJson client "v1/contacts/delete" (object ["email" .= email])
    Right userId ->
        postJson client "v1/contacts/delete" (object ["userId" .= userId])

-- ------------------------------------------------------------------
-- Contact properties & mailing lists
-- ------------------------------------------------------------------

createContactProperty :: LoopsClient -> Text -> Text -> IO Value
createContactProperty client name typ = do
    unless (typ `elem` ["string", "number", "boolean", "date"]) $ throwIO $ ValidationError "type must be one of 'string', 'number', 'boolean', 'date'."
    postJson client "v1/contacts/properties" (object ["name" .= name, "type" .= typ])

getContactProperties :: LoopsClient -> Text -> IO Value
getContactProperties client list_ = do
    unless (list_ `elem` ["all", "custom"]) $ throwIO $ ValidationError "list must be 'all' or 'custom'."
    get client "v1/contacts/properties" [("list", autoParam list_)]

getMailingLists :: LoopsClient -> IO Value
getMailingLists client = get client "v1/lists" []

-- ------------------------------------------------------------------
-- Events & transactional emails
-- ------------------------------------------------------------------

sendEvent ::
    LoopsClient ->
    -- | Name of the event to send
    Text ->
    -- | Email of the contact (either email or userId must be provided)
    Maybe Text ->
    -- | User ID of the contact (either email or userId must be provided)
    Maybe Text ->
    -- | Contact properties to update
    Maybe ContactProperties ->
    -- | Event-specific properties
    Maybe EventProperties ->
    -- | Mailing lists to subscribe the contact to
    Maybe [MailingList] ->
    -- | Additional HTTP headers
    Maybe [(BS8.ByteString, BS8.ByteString)] ->
    IO Value
sendEvent client eventName mEmail mUserId mContactProps mEventProps mMailingLists mHeaders = do
    unless (isJust mEmail || isJust mUserId) $
        throwIO $
            ValidationError "You must provide either 'email' or 'user_id'."
    postJsonWithHeaders
        client
        "v1/events/send"
        (buildSendEventPayload eventName mEmail mUserId mContactProps mEventProps mMailingLists)
        mHeaders

{- | Build the JSON payload that 'sendEvent' POSTs to @v1/events/send@.

The Loops Send Event API expects contact properties as top-level
attributes alongside @email@\/@eventName@, *not* nested under a
@contactProperties@ key (see <https://loops.so/docs/api-reference/send-event>:
"These should be added as top-level attributes in the request"). This
helper flattens the wrapped 'ContactProperties' object accordingly so
Loops recognises the standard names (@firstName@, @subscribed@, …) and
stores anything else as a custom contact property.

Exposed primarily so callers can assert on the assembled payload in
tests; production code should call 'sendEvent'.
-}
buildSendEventPayload ::
    -- | Event name
    Text ->
    -- | Email of the contact (optional)
    Maybe Text ->
    -- | User ID of the contact (optional)
    Maybe Text ->
    -- | Contact properties to update
    Maybe ContactProperties ->
    -- | Event-specific properties
    Maybe EventProperties ->
    -- | Mailing lists to subscribe the contact to
    Maybe [MailingList] ->
    Value
buildSendEventPayload eventName mEmail mUserId mContactProps mEventProps mMailingLists =
    let basePayload = ["eventName" .= eventName]

        contactPayload = case mContactProps of
            Just (ContactProperties o) -> KM.toList o
            Nothing -> []

        eventPayload = ["eventProperties" .= mEventProps | isJust mEventProps]

        mailingListsPayload = ["mailingLists" .= mMailingLists | isJust mMailingLists]

        identifierPayload =
            catMaybes
                [ ("email" .=) <$> mEmail
                , ("userId" .=) <$> mUserId
                ]
     in object $
            mconcat
                [ basePayload
                , contactPayload
                , eventPayload
                , mailingListsPayload
                , identifierPayload
                ]

sendTransactionalEmail :: (ToJSON a) => LoopsClient -> LoopsEmail a -> Maybe [(BS8.ByteString, BS8.ByteString)] -> IO Value
sendTransactionalEmail client email = postJsonWithHeaders client "v1/transactional" (toJSON email)

getTransactionalEmails :: LoopsClient -> Int -> Maybe Text -> IO Value
getTransactionalEmails client perPage mCursor = do
    when (perPage < 10 || perPage > 50) $ throwIO $ ValidationError "per_page must be between 10 and 50."
    let qs = ("perPage", Just . BS8.pack $ show perPage) : maybe [] (\cur -> [("cursor", Just $ BS8.pack $ T.unpack cur)]) mCursor
    get client "v1/transactional" qs
