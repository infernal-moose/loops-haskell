{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Internal implementation moved from Loops.
module Internal.Loops (
    LoopsClient (..),
    Attachment (..),
    LoopsEmail (..),
    RateLimitExceededError (..),
    APIError (..),
    ValidationError (..),
    emailPattern,
    validateEmail,
    perform,
    baseRequest,
    autoParam,
    get,
    postJson,
    postJsonWithHeaders,
    putJson,
    objectToPairs,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
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
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Simple (
    Request,
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
        attachments = case leAttachments of
            [] -> []
            xs -> ["attachments" .= xs]

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

emailPattern :: BS8.ByteString
emailPattern = BS8.pack "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

validateEmail :: Text -> IO ()
validateEmail e =
    let bs = BS8.pack (T.unpack e)
     in unless (bs RE.=~ emailPattern) $ throwIO $ ValidationError "'email' must be a valid email address."

-- ---------------------------------------------------------------------------
-- Loops API client (sync, http-simple based)
-- ---------------------------------------------------------------------------

newtype LoopsClient = LoopsClient
    { apiKey :: Text
    }

baseRequest :: LoopsClient -> Text -> Request
baseRequest (LoopsClient apiKey) path =
    let full = T.unpack $ "https://app.loops.so/api/" <> path
        req0 = case parseRequest full of
            Left err -> error $ "Invalid URL: " ++ show err
            Right r -> r
     in setRequestHeaders
            [ ("Authorization", BS8.pack $ "Bearer " ++ T.unpack apiKey)
            , ("Content-Type", "application/json")
            ]
            req0

perform :: (FromJSON a) => Request -> IO a
perform req = do
    resp <- httpLBS req
    let headerInt name def =
            case getResponseHeader name resp of
                (x : _) -> fromMaybe def (readMaybeBS x)
                _ -> def
        readMaybeBS bs = case reads (BS8.unpack bs) of
            [(n, "")] -> Just n
            _ -> Nothing
        decodeBody :: (FromJSON b) => LBS.ByteString -> IO b
        decodeBody body =
            if LBS.null body
                then case fromJSON Null of
                    Success v -> return v
                    Error e -> throwIO $ APIError status (String $ T.pack $ "Failed to decode empty JSON response: " ++ e)
                else case eitherDecode' body of
                    Left e -> throwIO $ APIError status (String $ T.pack $ "Failed to decode JSON response: " ++ e)
                    Right v -> return v
        status = getResponseStatusCode resp
    if status == 429
        then do
            let lim = headerInt "x-ratelimit-limit" 10
                rem_ = headerInt "x-ratelimit-remaining" 0
            throwIO $ RateLimitExceededError (lim, rem_)
        else
            if status >= 400
                then decodeBody (getResponseBody resp) >>= throwIO . APIError status
                else decodeBody (getResponseBody resp)

autoParam :: Text -> Maybe BS8.ByteString
autoParam v = Just (BS8.pack $ T.unpack v)

-- ---------------------------------------------------------------------------
-- Internal HTTP helpers
-- ---------------------------------------------------------------------------

get :: LoopsClient -> Text -> [(BS8.ByteString, Maybe BS8.ByteString)] -> IO Value
get client path qs = do
    let req = setRequestQueryString qs $ baseRequest client path
    perform req

postJson :: (ToJSON a) => LoopsClient -> Text -> a -> IO Value
postJson client path payload = postJsonWithHeaders client path payload Nothing

postJsonWithHeaders :: (ToJSON a) => LoopsClient -> Text -> a -> Maybe [(BS8.ByteString, BS8.ByteString)] -> IO Value
postJsonWithHeaders client path payload mHeaders = do
    let req0 = baseRequest client path
        req1 = setRequestMethod "POST" $ setRequestBodyJSON payload req0
        req2 = maybe req1 (\hdrs -> setRequestHeaders (map (first CI.mk) hdrs) req1) mHeaders
    perform req2

putJson :: (ToJSON a) => LoopsClient -> Text -> a -> IO Value
putJson client path payload = do
    let req = setRequestMethod "PUT" $ setRequestBodyJSON payload $ baseRequest client path
    perform req

objectToPairs :: Object -> [Pair]
objectToPairs = fmap (uncurry (.=)) . KM.toList
