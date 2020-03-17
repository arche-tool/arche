
module Main where

import Network.Wai.Handler.Warp
import Servant

import Handler.ORFit
import Handler.SubmitANG
import Type.API
import Util.OrphanInstances ()

api :: Proxy FullAPI
api = Proxy

wwwServer :: Server Raw
wwwServer = serveDirectoryWebApp "../app/build"

main :: IO ()
main = run 8080 . serve api $ (orFitAPI :<|> uploadEbsdAPI) :<|> wwwServer
