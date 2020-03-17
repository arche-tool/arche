{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.API
    ( API
    , FullAPI
    , ORFitAPI
    , UploadEbsdAPI
    ) where


import Servant
import Servant.Multipart
import qualified Data.ByteString.Lazy     as BSL

import Arche.Strategy.ORFitAll (OREvaluation)
import qualified Arche.Strategy.ORFitAll as OR

import Type.Storage (HashEBSD)

type FullAPI = API :<|> Raw

type API = "api" :>
  (    ORFitAPI
  :<|> UploadEbsdAPI
  )

type UploadEbsdAPI = "upload" :>
  (MultipartForm Mem (MultipartData Mem) :> Post '[JSON] NoContent)

type ORFitAPI = "orfit" :>
  (                               Get  '[JSON] [Either String OREvaluation]
  :<|> Capture "ang" HashEBSD  :> Get  '[JSON] (Either String OREvaluation)
  :<|> ReqBody '[JSON] OR.Cfg  :> Post '[JSON] NoContent
  )