{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns  #-}

module Main where

import Network.Wai.Handler.Warp
import Servant

import Handler.GetEBSD
import Handler.ORFit
import Handler.SubmitANG
import System.Environment
import Type.API
import Util.OrphanInstances ()
import Web.App
import Web.Config


type ArcheWeb = API :<|> App

archeWeb :: Proxy ArcheWeb
archeWeb = Proxy

main :: IO ()
main = do
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

        api :: Server API
        api = orFitAPI :<|> uploadEbsdAPI

    run 8080 . serve archeWeb $ api :<|> app 
