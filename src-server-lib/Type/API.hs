{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.API
    ( API
    , ORAPI
    , EBSDAPI
    , ArcheAPI
    , CacheHeader 
    ) where

import Data.Text (Text)
import Servant

import qualified Arche.Strategy.ORFitAll as OR

import Type.Storage (
  HashEBSD,
  HashOR,
  HashArche,
  StorageLink,
  StoragePublicLink,
  StorageObjectName
  )
import Type.Store (Arche, ArcheCfg, EBSD, OR)

type API = "api" :>
  (    EBSDAPI
  :<|> ORAPI
  :<|> ArcheAPI
  )

type CacheHeader v = Headers '[ Header "Cache-Control" String ] v

-- ==<< EBSD handling >>==
-- POST /ebsd ObjectName EBSD
-- GET  /ebsd [EBSD]
-- GET  /ebsd/hash/{ebsd_hash} EBSD
-- GET  /ebsd/upload-link StorageLink
type EBSDAPI = "ebsd" :>
  ( ReqBody '[JSON] StorageObjectName :> QueryParam "alias" Text :> Post '[JSON] EBSD
  :<|> "hash" :> Capture "ebsd_hash" HashEBSD                    :> Get  '[JSON] EBSD
  :<|> "upload-link"                                             :> Get  '[JSON] StorageLink
  :<|>                                                              Get  '[JSON] (CacheHeader [EBSD])
  )

-- ==<< Orientation relationship handling >>==
-- POST /ebsd/hash/{ebsd_hash}/orfit ORFitCfg ORFit
-- POST /ebsd/hash/{ebsd_hash}/orfit/async ORFitCfg Text
-- GET  /ebsd/hash/{ebsd_hash}/orfit [ORFit]
-- GET  /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash} ORFit
type ORAPI = "ebsd" :> "hash" :> Capture "ebsd_hash" HashEBSD
  :> "orfit" :>
  (                                          Get  '[JSON] (CacheHeader [OR])
  :<|> "hash" :> Capture "or_hash" HashOR :> Get  '[JSON] OR
  :<|>            ReqBody '[JSON] OR.Cfg  :> Post '[JSON] OR
  :<|> "async" :> ReqBody '[JSON] OR.Cfg  :> Post '[JSON] Text
  )


-- ==<< Reconstruction handling >>==
-- POST /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche ArcheCfg Arche
-- POST /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche/async ArcheCfg Text
-- GET  /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche [Arche]
-- GET  /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche/hash/{arche_hash} Arche
type ArcheAPI = "ebsd" :> "hash" :> Capture "ebsd_hash" HashEBSD
  :> "orfit" :> "hash" :> Capture "or_hash" HashOR :> "arche" :>
  (                                                  Get  '[JSON] (CacheHeader [Arche StoragePublicLink])
  :<|> "hash"  :> Capture "arche_hash" HashArche  :> Get  '[JSON] (Arche StoragePublicLink)
  :<|>            ReqBody '[JSON] ArcheCfg        :> Post '[JSON] (Arche StoragePublicLink)
  :<|> "async" :> ReqBody '[JSON] ArcheCfg        :> Post '[JSON] Text
  )