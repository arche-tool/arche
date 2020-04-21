{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Network.Wai.Handler.Warp
import Servant

import Handler.ORFit
import Handler.EBSDAPI
import System.Environment
import Type.API
import Type.Store (User(..))
import Util.OrphanInstances ()
import Web.App
import Web.Config


type ArcheWeb = API :<|> App

archeWeb :: Proxy ArcheWeb
archeWeb = Proxy

main :: IO ()
main = do

    let user = User "ze@gmail.com" (Just "zeze")
  
    args <- getArgs
    let
        configErr = error . ("Erro reading config file: \n" <>)
        assetsErr = error . ("Erro reading Elm asset file: \n" <>)
    (!config, !elmAssets) <- case args of
        [configFile] -> do
            config <- either configErr id <$> readArcherServerConfig configFile
            elmAssets <- either assetsErr id <$> readElmAppConfig (assetManifestFile config)
            return (config, elmAssets)
        _ -> error "One and just one argument is necessary: the filepath to the configuration file."
    let

        app :: Server App
        app = appServer (staticFolder config) elmAssets
        
        fake :: Server ArcheAPI
        fake = undefined

        api :: Server API
        api = ebsdApi user :<|> orApi user :<|> fake

    run 8080 . serve archeWeb $ api :<|> app 
