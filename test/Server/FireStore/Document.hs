module Server.FireStore.Document
  ( test
  ) where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.Tasty.QuickCheck as QC

import Util.FireStore.Value

test :: TestTree
test = testCase "Example test case" $ do
    -- assertion no. 1 (passes)
    2 + 2 @?= 4
    -- assertion no. 2 (fails)
    assertBool "the list is not empty" $ null [1]
    -- assertion no. 3 (would have failed, but won't be executed because
    -- the previous assertion has already failed)
    "foo" @?= "bar"

data Test deriving (Generic)
data Test0 = Test0A | Test0B deriving (Generic, Show)
data Test1 = Test1 Text Text deriving (Generic, Show)
data Test2 = Test2 {test2a :: Text, test2b :: Text} deriving (Generic, Show)
data Test3 = Test3 {test3a :: Test2, test3b :: Test1} deriving (Generic, Show)

instance Serialize Test
instance Serialize Test0
instance Serialize Test1
instance Serialize Test2
instance Serialize Test3

matchStringValue :: FireStore.Value -> String -> Bool
matchStringValue v s = (v ^. FireStore.vStringValue) == (Just . pack $ s) 

test = let
  [test1a, test1b] = (put $ Test1 "xx" "yy") ^. FireStore.vArrayValue . _Just . FireStore.avValues
  mapTest2 = (put $ Test2 "xx" "yy") ^. FireStore.vMapValue . _Just . FireStore.mvFields . _Just . FireStore.mvfAddtional
  in [
    matchStringValue (put Test0A) (show Test0A),
    matchStringValue test1a "xx",
    matchStringValue test1b "yy",
    matchStringValue (mapTest2 HM.! "test2a") "xx",
    matchStringValue (mapTest2 HM.! "test2b") "yy"
    ]