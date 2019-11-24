{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Person where

import GHC.Generics
import Aws.Lambda
import Data.Aeson
import Data.Map

import Type.APIGateway

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, FromJSON, ToJSON)

greet :: Person -> String
greet person = "Hello, " ++ name person ++ "!"

handler :: Event Person -> Context -> IO (Either String (Response String))
handler Event{..} _ = do
  putStrLn (show resource)
  let person = body
  pure $ Right Response
    { statusCode = 200
    , body = greet person
    , isBase64Encoded = False
    , headers = Data.Map.singleton "Content-Type" "application/json"
    }