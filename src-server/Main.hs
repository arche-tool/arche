
module Main where

import Network.Wai.Handler.Warp
import Servant

import Handler.ORFit
import Type.API (API)
import Util.OrphanInstances ()

api :: Proxy API
api = Proxy

main :: IO ()
main = run 8080 . serve api $ orFitAPI
