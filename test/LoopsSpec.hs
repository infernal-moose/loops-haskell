{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for the Loops module.
module Main (main) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
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
