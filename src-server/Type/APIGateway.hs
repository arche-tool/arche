{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Type.APIGateway
    ( Event(..)
    , Response(..)
    ) where

import GHC.Generics
import Data.Aeson
import Data.Map

-- ============================== Input event ===============================

{--
Defined on:
https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html#api-gateway-simple-proxy-for-lambda-input-format
--}

data Method
  = GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Event a = Event
  { resource :: String
  , path :: String
  , body :: a
  , httpMethod :: Method
  , headers :: Map String String
  , pathParameters :: Map String String
  , isBase64Encoded :: Bool
  } deriving (Generic, FromJSON)


-- ============================== Response wrapper ===============================

data Response a = Response
  { statusCode:: Int
  , body :: a
  , isBase64Encoded :: Bool
  , headers :: Map String String
  } deriving (Generic, ToJSON)
