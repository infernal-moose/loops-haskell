{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for the Loops module.
module Main (main) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.Text (Text)
import Loops
import Test.Hspec

{- | Convenience predicate for checking that a given IO action throws a
  'ValidationError'.
-}
shouldThrowValidationError :: IO a -> Expectation
shouldThrowValidationError action =
    action `shouldThrow` \(ValidationError _) -> True

main :: IO ()
main = hspec $ do
    describe "LoopsEmail JSON encoding" $ do
        it "converts to JSON correctly" $ do
            let attachment =
                    Attachment
                        { filename = "test.pdf"
                        , contentType = "application/pdf"
                        , data_ = "base64data"
                        }
                email =
                    LoopsEmail
                        { leEmail = "test@example.com"
                        , leTransactionalId = "test-id"
                        , leAddToAudience = Just True
                        , leDataVariables = Just $ KM.fromList [(K.fromText "name", String "John")]
                        , leAttachments = [attachment]
                        }
                expected :: Value
                expected =
                    object
                        [ "email" .= ("test@example.com" :: Text)
                        , "transactionalId" .= ("test-id" :: Text)
                        , "addToAudience" .= True
                        , "dataVariables" .= object ["name" .= ("John" :: Text)]
                        , "attachments"
                            .= [ object
                                    [ "filename" .= ("test.pdf" :: Text)
                                    , "contentType" .= ("application/pdf" :: Text)
                                    , "data" .= ("base64data" :: Text)
                                    ]
                               ]
                        ]
            toJSON email `shouldBe` expected

        it "omits attachments key when list is empty" $ do
            let email :: LoopsEmail (KM.KeyMap Value)
                email =
                    LoopsEmail
                        { leEmail = "test@example.com"
                        , leTransactionalId = "test-id"
                        , leAddToAudience = Nothing
                        , leDataVariables = Nothing
                        , leAttachments = []
                        }
            case toJSON email of
                Object o -> KM.lookup (K.fromText "attachments") o `shouldBe` Nothing
                _ -> expectationFailure "Expected LoopsEmail to encode to a JSON object"

    describe "Client-side validation helpers" $ do
        it "createContact rejects invalid email" $ do
            let client = LoopsClient "dummy-key"
            shouldThrowValidationError $ createContact client "not-an-email" Nothing Nothing

        it "updateContact rejects invalid email" $ do
            let client = LoopsClient "dummy-key"
            shouldThrowValidationError $ updateContact client "no-at" KM.empty Nothing

        it "sendEvent requires at least one identifier" $ do
            let client = LoopsClient "dummy-key"
            shouldThrowValidationError $ sendEvent client "test_event" Nothing Nothing Nothing Nothing Nothing Nothing

        it "getTransactionalEmails enforces perPage bounds" $ do
            let client = LoopsClient "dummy-key"
            shouldThrowValidationError $ getTransactionalEmails client 5 Nothing
            shouldThrowValidationError $ getTransactionalEmails client 100 Nothing

        it "findContact rejects invalid email" $ do
            let client = LoopsClient "dummy-key"
            shouldThrowValidationError (findContact client (Left "bad-email" :: Either Text Text))

        it "deleteContact rejects invalid email" $ do
            let client = LoopsClient "dummy-key"
            shouldThrowValidationError (deleteContact client (Left "bad-email" :: Either Text Text))

        it "createContactProperty rejects invalid type" $ do
            let client = LoopsClient "dummy-key"
            shouldThrowValidationError (createContactProperty client "prop" "integer")

        it "getContactProperties rejects invalid list param" $ do
            let client = LoopsClient "dummy-key"
            shouldThrowValidationError (getContactProperties client "unknown")

    describe "Webhook event parsing" $ do
        it "parses testing.testEvent" $ do
            let json =
                    "{ \"eventName\": \"testing.testEvent\""
                        <> ", \"eventTime\": 1700000000"
                        <> ", \"webhookSchemaVersion\": \"1.0\""
                        <> ", \"message\": \"Hello from Loops\" }"
            case parseWebhookEvent json of
                Left err -> expectationFailure err
                Right evt -> do
                    weEventTime evt `shouldBe` 1700000000
                    weWebhookSchemaVersion evt `shouldBe` "1.0"
                    case wePayload evt of
                        TestEvent msg -> msg `shouldBe` "Hello from Loops"
                        other -> expectationFailure $ "Expected TestEvent, got: " ++ show other

        it "parses contact.unsubscribed" $ do
            let json =
                    "{ \"eventName\": \"contact.unsubscribed\""
                        <> ", \"eventTime\": 1700000001"
                        <> ", \"webhookSchemaVersion\": \"1.0\""
                        <> ", \"contactIdentity\": { \"id\": \"c1\", \"email\": \"a@b.com\", \"userId\": \"u1\" } }"
            case parseWebhookEvent json of
                Left err -> expectationFailure err
                Right evt -> case wePayload evt of
                    ContactUnsubscribed ci -> do
                        ciId ci `shouldBe` "c1"
                        ciEmail ci `shouldBe` "a@b.com"
                        ciUserId ci `shouldBe` Just "u1"
                    other -> expectationFailure $ "Expected ContactUnsubscribed, got: " ++ show other

        it "parses email.unsubscribed" $ do
            let json =
                    "{ \"eventName\": \"email.unsubscribed\""
                        <> ", \"eventTime\": 1700000002"
                        <> ", \"webhookSchemaVersion\": \"1.0\""
                        <> ", \"sourceType\": \"campaign\""
                        <> ", \"campaignId\": \"camp1\""
                        <> ", \"email\": { \"id\": \"e1\", \"emailMessageId\": \"msg1\", \"subject\": \"Hello\" }"
                        <> ", \"contactIdentity\": { \"id\": \"c2\", \"email\": \"b@c.com\" } }"
            case parseWebhookEvent json of
                Left err -> expectationFailure err
                Right evt -> case wePayload evt of
                    EmailUnsubscribed srcType campId emailRef ci -> do
                        srcType `shouldBe` "campaign"
                        campId `shouldBe` "camp1"
                        werId emailRef `shouldBe` "e1"
                        werEmailMessageId emailRef `shouldBe` "msg1"
                        werSubject emailRef `shouldBe` "Hello"
                        ciId ci `shouldBe` "c2"
                        ciEmail ci `shouldBe` "b@c.com"
                        ciUserId ci `shouldBe` Nothing
                    other -> expectationFailure $ "Expected EmailUnsubscribed, got: " ++ show other

        it "parses contact.mailingList.unsubscribed" $ do
            let json =
                    "{ \"eventName\": \"contact.mailingList.unsubscribed\""
                        <> ", \"eventTime\": 1700000003"
                        <> ", \"webhookSchemaVersion\": \"1.0\""
                        <> ", \"contactIdentity\": { \"id\": \"c3\", \"email\": \"c@d.com\" }"
                        <> ", \"mailingList\": { \"id\": \"ml1\", \"name\": \"Newsletter\", \"description\": \"Weekly\", \"isPublic\": true } }"
            case parseWebhookEvent json of
                Left err -> expectationFailure err
                Right evt -> case wePayload evt of
                    MailingListUnsubscribed ci ml -> do
                        ciId ci `shouldBe` "c3"
                        wmlId ml `shouldBe` "ml1"
                        wmlName ml `shouldBe` "Newsletter"
                        wmlDescription ml `shouldBe` Just "Weekly"
                        wmlIsPublic ml `shouldBe` True
                    other -> expectationFailure $ "Expected MailingListUnsubscribed, got: " ++ show other

        it "falls back to UnknownEvent for unrecognized event names" $ do
            let json =
                    "{ \"eventName\": \"some.future.event\""
                        <> ", \"eventTime\": 1700000004"
                        <> ", \"webhookSchemaVersion\": \"1.0\" }"
            case parseWebhookEvent json of
                Left err -> expectationFailure err
                Right evt -> case wePayload evt of
                    UnknownEvent name _ -> name `shouldBe` "some.future.event"
                    other -> expectationFailure $ "Expected UnknownEvent, got: " ++ show other

        it "returns Left for invalid JSON" $ do
            let json = "not valid json" :: LBS.ByteString
            parseWebhookEvent json `shouldSatisfy` isLeft

        it "returns Left for missing required fields" $ do
            let json = "{ \"eventName\": \"testing.testEvent\" }" :: LBS.ByteString
            parseWebhookEvent json `shouldSatisfy` isLeft

        it "parseWebhookEventValue works with a pre-parsed Value" $ do
            let val =
                    object
                        [ "eventName" .= ("testing.testEvent" :: Text)
                        , "eventTime" .= (1700000000 :: Int)
                        , "webhookSchemaVersion" .= ("1.0" :: Text)
                        , "message" .= ("test" :: Text)
                        ]
            case parseWebhookEventValue val of
                Left err -> expectationFailure err
                Right evt -> case wePayload evt of
                    TestEvent msg -> msg `shouldBe` "test"
                    other -> expectationFailure $ "Expected TestEvent, got: " ++ show other
