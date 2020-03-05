{-# LANGUAGE RecordWildCards #-}
module Main where

import Test.Tasty

import Arche.TestOR

main :: IO ()
main = defaultMain
     $ testGroup "Tests"
     [ Arche.TestOR.test
     ]
