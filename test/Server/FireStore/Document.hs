{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
module Server.FireStore.Document
  ( test
  ) where

import Control.Lens ((^.), _Just)
import Control.Monad (zipWithM_)
import Data.Text (pack)
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.HashMap.Strict      as HM
import qualified Network.Google.FireStore as FireStore

import Util.FireStore.Value

test :: TestTree
test = testGroup "FireStore Document generic instances"
  [ testGenericShape
  , testIsomorphism
  ]

data Test0 = Test0A | Test0B | Test0C deriving (Generic, Eq, Show)
data Test1 = Test1A String String | Test1B | Test1C String deriving (Generic, Eq, Show)
data Test2 = Test2A {test2a :: String, test2b :: String} | Test2B deriving (Generic, Eq, Show)
data Test3 = Test3A {test3a :: Maybe Test2, test3b :: Test1} deriving (Generic, Eq, Show)
data Test4 = Test4A String String deriving (Generic, Eq, Show)

instance ToDocValue Test0
instance ToDocValue Test1
instance ToDocValue Test2
instance ToDocValue Test3
instance ToDocValue Test4

instance FromDocValue Test0
instance FromDocValue Test1
instance FromDocValue Test2
instance FromDocValue Test3
instance FromDocValue Test4

matchStringValue :: FireStore.Value -> String -> Assertion
matchStringValue v s = (v ^. FireStore.vStringValue) @?= (Just . pack $ s) 

matchArrayValue :: FireStore.Value -> [String] -> Assertion
matchArrayValue v ss = zipWithM_ matchStringValue vs ss
  where
    vs = v ^. FireStore.vArrayValue . _Just . FireStore.avValues

testGenericShape :: TestTree
testGenericShape = testCase "Generic encode data shape" $ do
  let
    [test4a, test4b] = (toValue $ Test4A "xx" "yy") ^. FireStore.vArrayValue . _Just . FireStore.avValues
    mapTest1a = (toValue $ Test1A "xx" "yy") ^. FireStore.vMapValue . _Just . FireStore.mvFields . _Just . FireStore.mvfAddtional
    mapTest2b = (toValue $ Test2A "xx" "yy") ^. FireStore.vMapValue . _Just . FireStore.mvFields . _Just . FireStore.mvfAddtional
  matchStringValue (toValue Test0A) (show Test0A)
  matchStringValue test4a "xx"
  matchStringValue test4b "yy"
  matchStringValue (mapTest1a HM.! "__TAG__") "Test1A"
  matchArrayValue  (mapTest1a HM.! "__VALUES__") ["xx", "yy"]
  matchStringValue (mapTest2b HM.! "test2a") "xx"
  matchStringValue (mapTest2b HM.! "test2b") "yy"

checkIso :: (Show a, ToDocValue a, FromDocValue a, Eq a) => a -> Assertion
checkIso x = let
  regen = fromValue (toValue x)
  in case regen of
    Right v  -> v @?= x
    Left err -> error $ "original:\n" ++ show x ++ "\nparsed:\n" ++ show regen ++ "\nreason:\n" ++ err

testIsomorphism :: TestTree
testIsomorphism = testCase "Generic isomorphism test" $ do
  let
    _test1a = Test1A "ss" "dd"
    _test1b = Test1B
    _test1c = Test1C "ss"
    _test2a = Test2A "ss" "dd"
    _test3a = Test3A (Just _test2a) _test1a
    _test3b = Test3A Nothing _test1a
    _test3c = Test3A Nothing _test1b
    _test3d = Test3A (Just _test2a) _test1b
    _test4a = Test4A "ss" "dd"
  checkIso _test1a
  checkIso _test1b
  checkIso _test1c
  checkIso _test2a
  checkIso _test3a
  checkIso _test3b
  checkIso _test3c
  checkIso _test3d
  checkIso _test4a