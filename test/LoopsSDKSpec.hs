{-# LANGUAGE OverloadedStrings #-}

{- | Test suite for the LoopsSDK module.
  This mirrors the behaviour covered by the original Python tests but in
  idiomatic Haskell using the @hspec@ framework.
-}
module Main (main) where

import Control.Exception (evaluate)
import Data.Aeson (Object, Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import LoopsSDK
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
            let email =
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
            client <- newClient "dummy-key" Nothing
            shouldThrowValidationError $ createContact client "not-an-email" Nothing Nothing "dummy-key"

        it "updateContact rejects invalid email" $ do
            client <- newClient "dummy-key" Nothing
            shouldThrowValidationError $ updateContact client "no-at" KM.empty Nothing "dummy-key"

        it "sendEvent requires at least one identifier" $ do
            client <- newClient "dummy-key" Nothing
            shouldThrowValidationError $ sendEvent client "test_event" Nothing Nothing Nothing Nothing Nothing Nothing "dummy-key"

        it "getTransactionalEmails enforces perPage bounds" $ do
            client <- newClient "dummy-key" Nothing
            shouldThrowValidationError $ getTransactionalEmails client 5 Nothing "dummy-key"
            shouldThrowValidationError $ getTransactionalEmails client 100 Nothing "dummy-key"
