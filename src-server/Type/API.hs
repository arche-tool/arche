{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.API
    ( API
    , ORFitAPI
    ) where

import Servant

import Arche.Strategy.ORFitAll (OREvaluation)
import qualified Arche.Strategy.ORFitAll as OR

import Type.Storage (HashEBSD)

type API = ORFitAPI

type ORFitAPI = "orfit" :>
  (                               Get  '[JSON] [Either String OREvaluation]
  :<|> Capture "ang" HashEBSD  :> Get  '[JSON] (Either String OREvaluation)
  :<|> ReqBody '[JSON] OR.Cfg  :> Post '[JSON] NoContent
  )