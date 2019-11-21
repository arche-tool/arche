{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Person where

import GHC.Generics
import Aws.Lambda
import Data.Aeson
import Data.Map
import qualified Data.ByteString.Lazy.Char8 as ByteString

data Event = Event
  { resource :: String
  , body :: String
  } deriving (Generic, FromJSON)

data Response = Response
  { statusCode:: Int
  , body :: String
  , isBase64Encoded :: Bool
  , headers :: Map String String
  } deriving (Generic, ToJSON)

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, FromJSON, ToJSON)

greet :: Person -> String
greet person =
  "Hello, " ++ name person ++ "!"

handler :: Event -> Context -> IO (Either String Response)
handler Event{..} context = do
  putStrLn (show resource)
  putStrLn (show body)
  case decode (ByteString.pack body) of
    Just person ->
      pure $ Right Response
        { statusCode = 200
        , body = greet person
        , isBase64Encoded = False
        , headers = Data.Map.singleton "Content-Type" "application/json"
        }
    Nothing ->
      pure $ Right Response
        { statusCode = 200
        , body = "bad person"
        , isBase64Encoded = False
        , headers = Data.Map.singleton "Content-Type" "application/json"
        }