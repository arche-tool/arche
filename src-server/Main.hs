{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Network.Wai.Handler.Warp
import Servant

import Type.API
import Util.Auth            (AuthIDToken, authServerContext)
import Util.OrphanInstances ()
import qualified Type.Store as Store
import qualified Util.Auth  as Auth

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

    let
        oauthClientID = Auth.mkOAuthClientID (oauth_client_id config)

        fake :: Server ArcheAPI
        fake = undefined

        api :: Store.User -> Server API
        api user = ebsdApi user :<|> orApi user :<|> fake

        server :: Server ArcheServer
        server = \token -> api $ Store.User
            { Store.id_number = Auth.sub token
            , Store.email     = Auth.email token
            , Store.name      = Auth.name token
            }

    run (port config) $
        serveWithContext proxyServer (authServerContext oauthClientID) server
