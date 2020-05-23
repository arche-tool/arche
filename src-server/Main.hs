{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Network.Wai.Handler.Warp
import Servant
import System.Environment

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
  
    args <- getArgs
    let
        configErr = error . ("Erro reading config file: \n" <>)
    config <- case args of
        [configFile] -> do
            either configErr id <$> readArcherServerConfig configFile
        _ -> error "One and just one argument is necessary: the filepath to the configuration file."
    let

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

    run 8080 $ serveWithContext proxyServer authServerContext server
