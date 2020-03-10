{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.API where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Servant

import Arche.Strategy.ORFitAll (OREvaluation)

type API = SimpleAPI "users" User UserId

-- Three endpoints:
--   - GET /<name>
--   - GET /<name>/<some 'i'>
--   - POST /<name>
type SimpleAPI (name :: Symbol) a i = name :>
  (                         Get '[JSON] [a]
  :<|> Capture "id" i    :> Get '[JSON] (Either String OREvaluation)
  :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent
  )

type UserId = Int

data User = User { username :: String, age :: Int }
  deriving Generic

instance FromJSON User
instance ToJSON   User

type ProductId = Int

data Product = Product { productname :: String }
  deriving Generic

instance FromJSON Product
instance ToJSON   Product