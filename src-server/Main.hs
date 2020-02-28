{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.IO.Class (liftIO)

import Handler.ORFit
import Arche.Strategy.ORFitAll (OREvaluation)

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

api :: Proxy API
api = Proxy

main :: IO ()
main = run 8080 . serve api $ userServer

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

simpleServer
  :: Handler [a]
  -> (i -> Handler (Either String OREvaluation))
  -> (a -> Handler NoContent)
  -> Server (SimpleAPI name a i)
simpleServer listAs getA postA =
  listAs :<|> getA :<|> postA

userServer :: Server (SimpleAPI "users" User UserId)
userServer = simpleServer
  (return [])
  (\userid -> if userid > 1
    then return $ Left (show userid)
    else liftIO $ orFitHandler "FeNi.ang"
  )
  (\_user -> return NoContent)