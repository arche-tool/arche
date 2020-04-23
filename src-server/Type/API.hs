{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.API
    ( API
    , ORAPI
    , EBSDAPI
    , ArcheAPI
    ) where


import Servant
import Servant.Multipart
import qualified Data.ByteString.Lazy     as BSL

import Arche.Strategy.ORFitAll (OREvaluation)
import qualified Arche.Strategy.GomesGraph as Arche
import qualified Arche.Strategy.ORFitAll as OR

import Type.Storage (HashEBSD, HashOR, HashArche)
import Type.Store (Arche, EBSD, OR)

type API = "api" :>
  (    EBSDAPI
  :<|> ORAPI
  :<|> ArcheAPI
  )

-- ==<< EBSD handling >>==
-- POST /ebsd MultipartFile EBSD
-- GET /ebsd [EBSD]
-- GET /ebsd/hash/{ebsd_hash} EBSD
type EBSDAPI = "ebsd" :>
  (MultipartForm Mem (MultipartData Mem) :> Post '[JSON] [EBSD]
  :<|>                                      Get  '[JSON] [EBSD]
  :<|> Capture "hash" HashEBSD           :> Get  '[JSON] EBSD
  )

-- ==<< Orientation relationship handling >>==
-- POST /ebsd/hash/{ebsd_hash}/orfit ORFitCfg ORFit
-- GET /ebsd/hash/{ebsd_hash}/orfit [ORFit]
-- GET /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash} ORFit
type ORAPI = "ebsd" :> Capture "hash" HashEBSD :> "orfit" :>
  (                              Get  '[JSON] [OR]
  :<|> Capture "hash" HashOR  :> Get  '[JSON] OR
  :<|> ReqBody '[JSON] OR.Cfg :> Post '[JSON] NoContent
  )


-- ==<< Reconstruction handling >>==
-- POST /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche ArcheCfg Arche
-- GET /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche [Arche]
-- GET /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche/hash/{arche_hash} Arche
type ArcheAPI = "ebsd" :> Capture "hash" HashEBSD :> "orfit" :> Capture "hash" HashOR :> "arche" :>
  (                                 Get  '[JSON] [Arche]
  :<|> Capture "hash" HashArche  :> Get  '[JSON] Arche
  :<|> ReqBody '[JSON] Arche.Cfg :> Post '[JSON] NoContent
  )