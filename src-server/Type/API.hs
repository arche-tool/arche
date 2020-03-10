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
import Data.Text (Text)

import Arche.Strategy.ORFitAll (OREvaluation)
import qualified Arche.Strategy.ORFitAll as OR

type API = ORFitAPI

type ORFitAPI = "orfit" :>
  (                               Get  '[JSON] [Either String OREvaluation]
  :<|> Capture "ang" HashANG   :> Get  '[JSON] (Either String OREvaluation)
  :<|> ReqBody '[JSON] OR.Cfg  :> Post '[JSON] NoContent
  )

newtype HashANG = HashANG Text deriving (Show, Generic, Eq)
instance FromJSON HashANG
instance ToJSON   HashANG
instance FromHttpApiData HashANG where
    parseUrlPiece txt = Right $ HashANG txt

newtype StorageBucket = StorageBucket Text deriving (Show, Generic, Eq)
instance FromJSON StorageBucket
instance ToJSON   StorageBucket