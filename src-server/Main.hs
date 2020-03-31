{-# LANGUAGE TypeOperators     #-}

module Main where

import Network.Wai.Handler.Warp
import Servant

import Handler.ORFit
import Handler.SubmitANG
import Type.API
import Util.OrphanInstances ()
import Web.App


type ArcheWeb = API :<|> App

archeWeb :: Proxy ArcheWeb
archeWeb = Proxy

app :: Server App
app = appServer "/data/Edgar/arche/app/build"

api :: Server API
api = orFitAPI :<|> uploadEbsdAPI

main :: IO ()
main = run 8080 . serve archeWeb $ api :<|> app
