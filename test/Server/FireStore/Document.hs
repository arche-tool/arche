{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
module Server.FireStore.Document
  ( test
  ) where

import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Util.FireStore.Value

test :: TestTree
test = testGroup "FireStore Document generic instances"
  [ testGenericShape
  , testIsomorphism
  ]

data Test0 = Test0A | Test0B | Test0C deriving (Generic, Eq, Show)
data Test1 = Test1A String String | Test1B | Test1C String deriving (Generic, Eq, Show)
data Test2 = Test2A {test2a :: String, test2b :: String} | Test2B | Test2C deriving (Generic, Eq, Show)
data Test3 = Test3A {test3a :: Maybe Test2, test3b :: Test1} deriving (Generic, Eq, Show)
data Test4 = Test4A String String deriving (Generic, Eq, Show)
data Test5 = Test5A String String String String deriving (Show, Eq, Generic)
data Test6 = Test6A String String String String String deriving (Show, Eq, Generic)
newtype Test7 = Test7A Test5 deriving (Show, Eq, Generic)

instance ToDocValue Test0
instance ToDocValue Test1
instance ToDocValue Test2
instance ToDocValue Test3
instance ToDocValue Test4
instance ToDocValue Test5
instance ToDocValue Test6
instance ToDocValue Test7

instance FromDocValue Test0
instance FromDocValue Test1
instance FromDocValue Test2
instance FromDocValue Test3
instance FromDocValue Test4
instance FromDocValue Test5
instance FromDocValue Test6
instance FromDocValue Test7


matchParser :: (Show a, Eq a, ToDocValue a) => a -> Parser a -> Assertion
matchParser ref p = case runParser p (toValue ref) of 
  Right x  -> x @?= ref
  Left err -> fail err

testGenericShape :: TestTree
testGenericShape = testCase "Generic encode data shape" $ do
  let
  
  matchParser (Test4A "xx" "yy") $ do
    Test4A <$> parseArrayHeadWith parse <*> parseArrayHeadWith parse
  
  matchParser Test0A $ do
    tag <- parse >>= (.: "Test0A") 
    case tag of
      "Test0A" -> pure Test0A
      _        -> fail $ "other tag " ++ tag
  
  matchParser (Test2A "ee" "ff") $ do
    parse >>= (.: "Test2A") >>= \v -> Test2A
         <$> v .: "test2a"
         <*> v .: "test2b"
  
  matchParser Test2B $ do
    x <- parse >>= (.: "Test2B")
    if x == ("Test2B" :: String)
      then pure Test2B
      else pure Test2C 

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
    _test5a = Test5A "0" "1" "2" "3"
    _test6a = Test6A "0" "1" "2" "3" "4"
    _test7a = Test7A _test5a
  checkIso _test1a
  checkIso _test1b
  checkIso _test1c
  checkIso _test2a
  checkIso _test3a
  checkIso _test3b
  checkIso _test3c
  checkIso _test3d
  checkIso _test4a
  checkIso _test5a
  checkIso _test6a
  checkIso _test7a