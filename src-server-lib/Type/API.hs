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

import qualified Arche.Strategy.GomesGraph as Arche
import qualified Arche.Strategy.ORFitAll as OR

import Type.Storage (HashEBSD, HashOR, HashArche, StorageLink, StorageObjectName)
import Type.Store (Arche, EBSD, OR)

type API = "api" :>
  (    EBSDAPI
  :<|> ORAPI
  :<|> ArcheAPI
  )

-- ==<< EBSD handling >>==
-- POST /ebsd ObjectName EBSD
-- GET /ebsd [EBSD]
-- GET /ebsd/hash/{ebsd_hash} EBSD
-- GET /ebsd/upload-link StorageLink
type EBSDAPI = "ebsd" :>
  ( ReqBody '[JSON] StorageObjectName    :> Post '[JSON] EBSD
  :<|>                                      Get  '[JSON] [EBSD]
  :<|> Capture "hash" HashEBSD           :> Get  '[JSON] EBSD
  :<|> "upload-link"                     :> Get  '[JSON] StorageLink
  )

-- ==<< Orientation relationship handling >>==
-- POST /ebsd/hash/{ebsd_hash}/orfit ORFitCfg ORFit
-- GET /ebsd/hash/{ebsd_hash}/orfit [ORFit]
-- GET /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash} ORFit
type ORAPI = "ebsd" :> Capture "hash" HashEBSD :> "orfit" :>
  (                              Get  '[JSON] [OR]
  :<|> Capture "hash" HashOR  :> Get  '[JSON] OR
  :<|> ReqBody '[JSON] OR.Cfg :> Post '[JSON] OR
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