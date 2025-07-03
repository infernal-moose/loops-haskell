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
import Control.Monad (when, unless)
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
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Bifunctor (first)
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
data LoopsEmail = LoopsEmail
    { leEmail :: Text
    , leTransactionalId :: Text
    , leAddToAudience :: Maybe Bool
    , leDataVariables :: Maybe Object
    , leAttachments :: [Attachment]
    }
    deriving (Show, Eq, Generic)

instance ToJSON LoopsEmail where
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
newtype LoopsClient = LoopsClient
    { _apiRoot :: Text -- ensures trailing '/'
    }

-- | Construct a new 'LoopsClient' with the given API key and optional API root.
newClient :: Text -> Maybe Text -> IO LoopsClient
newClient apiKey mRoot = do
    let root = ensureSlash $ Data.Maybe.fromMaybe "https://app.loops.so/api/" mRoot
    pure (LoopsClient root)
  where
    ensureSlash t = if T.isSuffixOf "/" t then t else T.snoc t '/'

-- | Produce a fully-formed 'Request' with common headers applied.
baseRequest :: Text -> Text -> Text -> Request
baseRequest apiKey root path =
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
perform :: (FromJSON a) => Text -> Request -> IO a
perform apiKey req = do
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

testApiKey :: LoopsClient -> Text -> IO Value
testApiKey (LoopsClient root) apiKey =
    perform apiKey (baseRequest apiKey root "v1/api-key")

createContact :: LoopsClient -> Text -> Maybe Object -> Maybe Object -> Text -> IO Value
createContact c email props mailingLists apiKey = do
    validateEmail email
    let payload =
            object
                ( ["email" .= email]
                    ++ maybe [] objectToPairs props
                    ++ maybe [] ((: []) . ("mailingLists" .=)) mailingLists
                )
    postJson c "v1/contacts/create" payload apiKey

updateContact :: LoopsClient -> Text -> Object -> Maybe Object -> Text -> IO Value
updateContact c email props mailingLists apiKey = do
    validateEmail email
    let payload =
            object
                ( ["email" .= email]
                    ++ objectToPairs props
                    ++ maybe [] ((: []) . ("mailingLists" .=)) mailingLists
                )
    putJson c "v1/contacts/update" payload apiKey

findContact :: LoopsClient -> Maybe Text -> Maybe Text -> Text -> IO Value
findContact c mEmail mUserId apiKey = do
    when (Data.Maybe.isJust mEmail && Data.Maybe.isJust mUserId) $ throwIO $ ValidationError "Only one of 'email' or 'user_id' may be provided."
    when (Data.Maybe.isNothing mEmail && Data.Maybe.isNothing mUserId) $ throwIO $ ValidationError "You must provide either 'email' or 'user_id'."
    let qs = catMaybes [("email",) . Just . BS8.pack . T.unpack <$> mEmail, ("userId",) . Just . BS8.pack . T.unpack <$> mUserId]
    get c "v1/contacts/find" qs apiKey

deleteContact :: LoopsClient -> Maybe Text -> Maybe Text -> Text -> IO Value
deleteContact c mEmail mUserId apiKey = do
    when (Data.Maybe.isJust mEmail && Data.Maybe.isJust mUserId) $ throwIO $ ValidationError "Only one of 'email' or 'user_id' may be provided."
    when (Data.Maybe.isNothing mEmail && Data.Maybe.isNothing mUserId) $ throwIO $ ValidationError "You must provide either 'email' or 'user_id'."
    let payload = object $ catMaybes [("email" .=) <$> mEmail, ("userId" .=) <$> mUserId]
    postJson c "v1/contacts/delete" payload apiKey

-- ------------------------------------------------------------------
-- Contact properties & mailing lists
-- ------------------------------------------------------------------

createContactProperty :: LoopsClient -> Text -> Text -> Text -> IO Value
createContactProperty c name typ apiKey = do
    unless (typ `elem` ["string", "number", "boolean", "date"]) $ throwIO $ ValidationError "type must be one of 'string', 'number', 'boolean', 'date'."
    postJson c "v1/contacts/properties" (object ["name" .= name, "type" .= typ]) apiKey

autoParam :: Text -> Maybe BS8.ByteString
autoParam v = Just (BS8.pack $ T.unpack v)

getCustomProperties :: LoopsClient -> Text -> Text -> IO Value
getCustomProperties c list_ apiKey = do
    unless (list_ `elem` ["all", "custom"]) $
        throwIO $ ValidationError "list must be 'all' or 'custom'."
    get c "v1/contacts/properties" [("list", autoParam list_)] apiKey

getMailingLists :: LoopsClient -> Text -> IO Value
getMailingLists c apiKey = get c "v1/lists" [] apiKey

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
    Text ->
    IO Value
sendEvent c eventName mEmail mUserId mContactProps mEventProps mMailingLists mHeaders apiKey = do
    unless (Data.Maybe.isJust mEmail || Data.Maybe.isJust mUserId) $ throwIO $ ValidationError "You must provide either 'email' or 'user_id'."
    let payload =
            object $
                ["eventName" .= eventName]
                    ++ maybe [] objectToPairs mContactProps
                    ++ ["eventProperties" .= mEventProps]
                    ++ ["mailingLists" .= mMailingLists]
                    ++ catMaybes [("email" .=) <$> mEmail, ("userId" .=) <$> mUserId]
    postJsonWithHeaders c "v1/events/send" payload mHeaders apiKey

sendTransactionalEmail :: LoopsClient -> LoopsEmail -> Maybe [(BS8.ByteString, BS8.ByteString)] -> Text -> IO Value
sendTransactionalEmail c email =
    postJsonWithHeaders c "v1/transactional" (toJSON email)

getTransactionalEmails :: LoopsClient -> Int -> Maybe Text -> Text -> IO Value
getTransactionalEmails c perPage mCursor apiKey = do
    when (perPage < 10 || perPage > 50) $ throwIO $ ValidationError "per_page must be between 10 and 50."
    let qs = ("perPage", Just . BS8.pack $ show perPage) : maybe [] (\cur -> [("cursor", Just $ BS8.pack $ T.unpack cur)]) mCursor
    get c "v1/transactional" qs apiKey

-- ---------------------------------------------------------------------------
-- Internal HTTP helpers
-- ---------------------------------------------------------------------------

get :: LoopsClient -> Text -> [(BS8.ByteString, Maybe BS8.ByteString)] -> Text -> IO Value
get (LoopsClient root) path qs apiKey = do
    let req =
            setRequestQueryString qs $
                baseRequest apiKey root path
    perform apiKey req

postJson :: (ToJSON a) => LoopsClient -> Text -> a -> Text -> IO Value
postJson c path payload = postJsonWithHeaders c path payload Nothing

postJsonWithHeaders :: (ToJSON a) => LoopsClient -> Text -> a -> Maybe [(BS8.ByteString, BS8.ByteString)] -> Text -> IO Value
postJsonWithHeaders (LoopsClient root) path payload mHeaders apiKey = do
    let req0 = baseRequest apiKey root path
        req1 = setRequestMethod "POST" $ setRequestBodyJSON payload req0
        req2 = maybe req1 (\hdrs -> setRequestHeaders (map (Data.Bifunctor.first CI.mk) hdrs) req1) mHeaders
    perform apiKey req2

putJson :: (ToJSON a) => LoopsClient -> Text -> a -> Text -> IO Value
putJson (LoopsClient root) path payload apiKey = do
    let req =
            setRequestMethod "PUT" $
                setRequestBodyJSON payload $
                    baseRequest apiKey root path
    perform apiKey req

objectToPairs :: Object -> [Pair]
objectToPairs = fmap (uncurry (.=)) . KM.toList
