module Main where

import Test.Tasty

import Arche.TestOR
import Server.FireStore.Document

main :: IO ()
main = defaultMain
     $ testGroup "Tests"
     [ Arche.TestOR.test
     , Server.FireStore.Document.test
     ]
