{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

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
    getContactProperties,
    getMailingLists,

    -- * Events & transactional emails
    sendEvent,
    sendTransactionalEmail,
    getTransactionalEmails,
)
where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.Aeson (
    Object,
    ToJSON (toJSON),
    Value (..),
    object,
    (.=),
 )
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Internal.LoopsSDK

-- | Construct a new 'LoopsClient' with the given API key and optional API root.
newClient :: Text -> Maybe Text -> IO LoopsClient
newClient apiKey mRoot = do
    let root = ensureSlash $ Data.Maybe.fromMaybe "https://app.loops.so/api/" mRoot
    pure (LoopsClient apiKey root)
  where
    ensureSlash t = if T.isSuffixOf "/" t then t else T.snoc t '/'

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
