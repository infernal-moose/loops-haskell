{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : LoopsSDK
Description : Haskell SDK for Loops (https://loops.so)

This implementation mirrors the public interface of the official TypeScript SDK
and the Python reference implementation bundled with this repository.
All endpoints, error-handling behaviour, and payload shapes follow the public
REST documentation.

The module provides a lightweight synchronous client implemented with
@http-conduit@ (via @http-simple@) and uses @aeson@ for JSON serialisation.
-}
module LoopsSDK (
    -- * Core client & errors
    LoopsClient,
    newClient,
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

    -- * Contact properties & mailing lists
    createContactProperty,
    getCustomProperties,
    getMailingLists,

    -- * Events & transactional emails
    sendEvent,
    sendTransactionalEmail,
    getTransactionalEmails,
)
where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Data.Aeson (
    FromJSON,
    Object,
    Result (..),
    ToJSON (toJSON),
    Value (..),
    eitherDecode',
    fromJSON,
    object,
    (.=),
 )
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Simple (
    Request,
    Response,
    getResponseBody,
    getResponseHeader,
    getResponseStatusCode,
    httpLBS,
    parseRequest,
    setRequestBodyJSON,
    setRequestHeaders,
    setRequestMethod,
    setRequestQueryString,
 )
import qualified Text.Regex.TDFA as RE

-- ---------------------------------------------------------------------------
-- Public error types
-- ---------------------------------------------------------------------------

-- | Raised when the Loops API returns HTTP 429.
newtype RateLimitExceededError = RateLimitExceededError {unRateLimitExceeded :: (Int, Int)}
    deriving (Show, Generic, Exception)

{- | Raised when the Loops API returns any non-2xx response (except 429).
Carries the HTTP status code and the decoded JSON payload (if any).
-}
data APIError = APIError
    { apiErrorStatus :: Int
    , apiErrorJson :: Value
    }
    deriving (Show, Generic, Exception)

-- | Raised when the SDK detects invalid arguments before making a request.
newtype ValidationError = ValidationError Text
    deriving (Show, Generic, Exception)

-- ---------------------------------------------------------------------------
-- Data models
-- ---------------------------------------------------------------------------

{- | Represents a single file attachment for a transactional email.
The payload (@data@) must be Base-64 encoded to comply with Loops API.

We rely on deriving 'Generic' and provide a manual 'ToJSON' instance to
realise the expected camel-cased keys.

NOTE: The Python version marks the record immutable; in Haskell, all values
are immutable by default.
-}
data Attachment = Attachment
    { filename :: Text
    , contentType :: Text
    , data_ :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON Attachment where
    toJSON Attachment{..} =
        object
            [ "filename" .= filename
            , "contentType" .= contentType
            , "data" .= data_
            ]

{- | Container for all parameters required to send a transactional email.
Mirrors the Python 'LoopsEmail' dataclass exactly but in idiomatic Haskell.

Optional fields use 'Maybe'.

We again provide a custom 'ToJSON' instance to map snake_case -> camelCase
as expected by Loops.
-}
data LoopsEmail a = LoopsEmail
    { leEmail :: Text
    , leTransactionalId :: Text
    , leAddToAudience :: Maybe Bool
    , leDataVariables :: Maybe a
    , leAttachments :: [Attachment]
    }
    deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (LoopsEmail a) where
    toJSON LoopsEmail{..} =
        object $ base ++ catMaybes optional ++ attachments
      where
        base =
            [ "email" .= leEmail
            , "transactionalId" .= leTransactionalId
            ]
        optional =
            [ ("addToAudience" .=) <$> leAddToAudience
            , ("dataVariables" .=) <$> leDataVariables
            ]
        attachments =
            case leAttachments of
                [] -> []
                xs -> ["attachments" .= xs]

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

emailPattern :: BS8.ByteString
emailPattern = BS8.pack "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"

validateEmail :: Text -> IO ()
validateEmail e =
    let bs = BS8.pack (T.unpack e)
     in unless (bs RE.=~ emailPattern) $ throwIO $ ValidationError "'email' must be a valid email address."

-- ---------------------------------------------------------------------------
-- Loops API client (sync, http-simple based)
-- ---------------------------------------------------------------------------

{- | Lightweight synchronous Loops API client.
Mirrors the public surface of the official TypeScript SDK, but uses Haskell.
-}
data LoopsClient = LoopsClient
    { _apiKey :: Text
    , _apiRoot :: Text -- ensures trailing '/'
    }

-- | Construct a new 'LoopsClient' with the given API key and optional API root.
newClient :: Text -> Maybe Text -> IO LoopsClient
newClient apiKey mRoot = do
    let root = ensureSlash $ Data.Maybe.fromMaybe "https://app.loops.so/api/" mRoot
    pure (LoopsClient apiKey root)
  where
    ensureSlash t = if T.isSuffixOf "/" t then t else T.snoc t '/'

-- | Produce a fully-formed 'Request' with common headers applied.
baseRequest :: LoopsClient -> Text -> Request
baseRequest (LoopsClient apiKey root) path =
    let full = T.unpack $ root <> path
        req0 = case parseRequest full of
            Left e -> error $ "Invalid URL: " ++ show e
            Right r -> r
     in setRequestHeaders
            [ ("Authorization", BS8.pack $ "Bearer " ++ T.unpack apiKey)
            , ("Content-Type", "application/json")
            ]
            req0

-- | Perform the HTTP request and handle Loops-specific error conventions.
perform :: (FromJSON a) => Request -> IO a
perform req = do
    resp <- httpLBS req
    let headerInt name def =
            case getResponseHeader name resp of
                (x : _) -> Data.Maybe.fromMaybe def (readMaybeBS x)
                _ -> def
        readMaybeBS bs = case reads (BS8.unpack bs) of
            [(n, "")] -> Just n
            _ -> Nothing
        decodeBody :: (FromJSON a) => Response LBS.ByteString -> a
        decodeBody r =
            if LBS.length (getResponseBody r) > 0
                then case eitherDecode' (getResponseBody r) of
                    Right v -> v
                    Left _ -> error "Failed to decode JSON response"
                else case fromJSON Null of
                    Success v -> v
                    Error e -> error e
        status = getResponseStatusCode resp
    if status == 429
        then do
            let lim = headerInt "x-ratelimit-limit" 10
                rem_ = headerInt "x-ratelimit-remaining" 0
            throwIO $ RateLimitExceededError (lim, rem_)
        else
            if status >= 400
                then do
                    let body = decodeBody resp
                    throwIO $ APIError status body
                else pure (decodeBody resp)

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

findContact :: LoopsClient -> Maybe Text -> Maybe Text -> IO Value
findContact client mEmail mUserId = do
    when (Data.Maybe.isJust mEmail && Data.Maybe.isJust mUserId) $ throwIO $ ValidationError "Only one of 'email' or 'user_id' may be provided."
    when (Data.Maybe.isNothing mEmail && Data.Maybe.isNothing mUserId) $ throwIO $ ValidationError "You must provide either 'email' or 'user_id'."
    let qs = catMaybes [("email",) . Just . BS8.pack . T.unpack <$> mEmail, ("userId",) . Just . BS8.pack . T.unpack <$> mUserId]
    get client "v1/contacts/find" qs

deleteContact :: LoopsClient -> Maybe Text -> Maybe Text -> IO Value
deleteContact client mEmail mUserId = do
    when (Data.Maybe.isJust mEmail && Data.Maybe.isJust mUserId) $ throwIO $ ValidationError "Only one of 'email' or 'user_id' may be provided."
    when (Data.Maybe.isNothing mEmail && Data.Maybe.isNothing mUserId) $ throwIO $ ValidationError "You must provide either 'email' or 'user_id'."
    let payload = object $ catMaybes [("email" .=) <$> mEmail, ("userId" .=) <$> mUserId]
    postJson client "v1/contacts/delete" payload

-- ------------------------------------------------------------------
-- Contact properties & mailing lists
-- ------------------------------------------------------------------

createContactProperty :: LoopsClient -> Text -> Text -> IO Value
createContactProperty client name typ = do
    unless (typ `elem` ["string", "number", "boolean", "date"]) $ throwIO $ ValidationError "type must be one of 'string', 'number', 'boolean', 'date'."
    postJson client "v1/contacts/properties" (object ["name" .= name, "type" .= typ])

autoParam :: Text -> Maybe BS8.ByteString
autoParam v = Just (BS8.pack $ T.unpack v)

getCustomProperties :: LoopsClient -> Text -> IO Value
getCustomProperties client list_ = do
    unless (list_ `elem` ["all", "custom"]) $ throwIO $ ValidationError "list must be 'all' or 'custom'."
    get client "v1/contacts/properties" [("list", autoParam list_)]

getMailingLists :: LoopsClient -> IO Value
getMailingLists client = get client "v1/lists" []

-- ------------------------------------------------------------------
-- Events & transactional emails
-- ------------------------------------------------------------------

sendEvent ::
    LoopsClient ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Object ->
    Maybe Object ->
    Maybe Object ->
    Maybe [(BS8.ByteString, BS8.ByteString)] ->
    IO Value
sendEvent client eventName mEmail mUserId mContactProps mEventProps mMailingLists mHeaders = do
    unless (Data.Maybe.isJust mEmail || Data.Maybe.isJust mUserId) $ throwIO $ ValidationError "You must provide either 'email' or 'user_id'."
    let payload =
            object $
                ["eventName" .= eventName]
                    ++ maybe [] objectToPairs mContactProps
                    ++ ["eventProperties" .= mEventProps]
                    ++ ["mailingLists" .= mMailingLists]
                    ++ catMaybes [("email" .=) <$> mEmail, ("userId" .=) <$> mUserId]
    postJsonWithHeaders client "v1/events/send" payload mHeaders

sendTransactionalEmail :: (ToJSON a) => LoopsClient -> LoopsEmail a -> Maybe [(BS8.ByteString, BS8.ByteString)] -> IO Value
sendTransactionalEmail client email = postJsonWithHeaders client "v1/transactional" (toJSON email)

getTransactionalEmails :: LoopsClient -> Int -> Maybe Text -> IO Value
getTransactionalEmails client perPage mCursor = do
    when (perPage < 10 || perPage > 50) $ throwIO $ ValidationError "per_page must be between 10 and 50."
    let qs = ("perPage", Just . BS8.pack $ show perPage) : maybe [] (\cur -> [("cursor", Just $ BS8.pack $ T.unpack cur)]) mCursor
    get client "v1/transactional" qs

-- ---------------------------------------------------------------------------
-- Internal HTTP helpers
-- ---------------------------------------------------------------------------

get :: LoopsClient -> Text -> [(BS8.ByteString, Maybe BS8.ByteString)] -> IO Value
get client path qs = do
    let req =
            setRequestQueryString qs $
                baseRequest client path
    perform req

postJson :: (ToJSON a) => LoopsClient -> Text -> a -> IO Value
postJson client path payload = postJsonWithHeaders client path payload Nothing

postJsonWithHeaders :: (ToJSON a) => LoopsClient -> Text -> a -> Maybe [(BS8.ByteString, BS8.ByteString)] -> IO Value
postJsonWithHeaders client path payload mHeaders = do
    let req0 = baseRequest client path
        req1 = setRequestMethod "POST" $ setRequestBodyJSON payload req0
        req2 = maybe req1 (\hdrs -> setRequestHeaders (map (Data.Bifunctor.first CI.mk) hdrs) req1) mHeaders
    perform req2

putJson :: (ToJSON a) => LoopsClient -> Text -> a -> IO Value
putJson client path payload = do
    let req =
            setRequestMethod "PUT" $
                setRequestBodyJSON payload $
                    baseRequest client path
    perform req

objectToPairs :: Object -> [Pair]
objectToPairs = fmap (uncurry (.=)) . KM.toList
