{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Network.Wai.Handler.Warp
import Servant

import Type.API
import Util.Auth            (AuthIDToken, authServerContext)
import Util.Storage         (loadStorageSigner)
import Util.OrphanInstances ()
import qualified Type.Store as Store
import qualified Util.Auth  as Auth

import Handler.ArcheAPI
import Handler.ORAPI
import Handler.EBSDAPI
import Server.Config

type ArcheServer = AuthIDToken :> API

proxyServer :: Proxy ArcheServer
proxyServer = Proxy

main :: IO ()
main = do
  
    config <- loadConfig
    putStrLn $ "Starting server with the following configuration: " ++ show config

    storageSigner <- loadStorageSigner (signerEncodedCredentials config)

    let
        oauthClientID = Auth.mkOAuthClientID (oauth_client_id config)

        api :: Auth.BearerToken -> Store.User -> Server API
        api tk user = ebsdApi storageSigner user :<|> orApi user :<|> archeApi tk user

        server :: Server ArcheServer
        server = \(tk, info) -> let
            user = Store.User
                { Store.id_number = Auth.sub   info
                , Store.email     = Auth.email info
                , Store.name      = Auth.name  info
                }
            in api tk user

    run (port config) $ do
        serveWithContext proxyServer (authServerContext oauthClientID) server
