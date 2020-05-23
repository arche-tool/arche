{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Network.Wai.Handler.Warp
import Servant
import System.Environment

import Type.API
import Type.Store (User(..))
import Util.Auth
import Util.OrphanInstances ()

import Handler.ORAPI
import Handler.EBSDAPI
import Server.Config

type ArcheServer = AuthIDToken :> API

proxyServer :: Proxy ArcheServer
proxyServer = Proxy

main :: IO ()
main = do

    let user = User "ze@gmail.com" (Just "zeze")
  
    args <- getArgs
    let
        configErr = error . ("Erro reading config file: \n" <>)
    config <- case args of
        [configFile] -> do
            config <- either configErr id <$> readArcherServerConfig configFile
            return config
        _ -> error "One and just one argument is necessary: the filepath to the configuration file."
    let

        fake :: Server ArcheAPI
        fake = undefined

        api :: Server API
        api = ebsdApi user :<|> orApi user :<|> fake

        server :: Server ArcheServer
        server = \token -> api

    run 8080 $ serveWithContext proxyServer authServerContext server
