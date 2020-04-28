{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Util.FireStore.Value
  ( ToDocValue(..)
  , FromDocValue(..)
  ) where

import Control.Applicative ((<|>), liftA2)
import Control.Lens        ((&), (?~), (.~), (^.), (^?), _Just)
import Data.Text           (Text, pack, unpack)
import GHC.Generics

import Data.Aeson

import qualified Data.HashMap.Strict      as HM
import qualified Network.Google.FireStore as FireStore

type ValueBuilder = Either [FireStore.Value] (HM.HashMap Text FireStore.Value)

-- =============================================================
-- ============== Exposed FromDocValue class  ================== 
-- =============================================================
class FromDocValue a where
  fromValue :: FireStore.Value -> Either String a

  default fromValue :: (Generic a, GFromDocumentValue (Rep a)) => FireStore.Value -> Either String a 
  fromValue = fmap to . gfromDocumentFields Nothing . fromArrayOrMapOrValue 

instance FromDocValue Text where
  fromValue value = maybe (Left "not a string value") Right (value ^. FireStore.vStringValue)

instance FromDocValue String where
  fromValue = fmap unpack . fromValue

class GFromDocumentValue f where
  gfromDocumentFields :: Maybe String -> ValueBuilder -> Either String (f a)

instance (GFromDocumentValue a, GFromDocumentValue b) => GFromDocumentValue (a :*: b) where
  gfromDocumentFields k x = case x of
    Left (v:vs) -> do
      a <- gfromDocumentFields k (Left [v])
      b <- gfromDocumentFields k (Left vs)
      return (a :*: b)
    hm -> do
      a <- gfromDocumentFields k hm
      b <- gfromDocumentFields k hm
      return (a :*: b)

instance (GFromDocumentValue a, GFromDocumentValue b) => GFromDocumentValue (a :+: b) where
  gfromDocumentFields k x = 
      (R1 <$> gfromDocumentFields k x) <|> (L1 <$>gfromDocumentFields k x) 

instance (GFromDocumentValue a, Datatype c) => GFromDocumentValue (D1 c a) where
  gfromDocumentFields _ x = M1 <$> (gfromDocumentFields Nothing x)

instance (GFromDocumentValue a, Constructor c) => GFromDocumentValue (C1 c a) where
  gfromDocumentFields _ x
    | isrec     = getTag cname x >>= fmap M1 . gfromDocumentFields Nothing
    | otherwise = getTag cname x >>= fmap M1 . gfromDocumentFields (Just cname)
    where
      isrec = conIsRecord (undefined :: M1 _i c _f _p)
      cname = conName (undefined :: M1 _i c _f _p)

instance (GFromDocumentValue a, Selector c) => GFromDocumentValue (S1 c a) where
  gfromDocumentFields _ x = M1 <$> case x of
    Right hm -> maybe 
      (Left $ "missing field: " ++ fieldName)
      (gfromDocumentFields Nothing . fromArrayOrMapOrValue)
      (HM.lookup (pack fieldName) hm)
    Left [v] -> gfromDocumentFields (Just fieldName) (fromArrayOrMapOrValue v)
    Left _   -> Left "unexpected multi-value record field"
    where fieldName = selName (undefined :: M1 _i c _f _p)

instance (FromDocValue a) => GFromDocumentValue (K1 i a) where
  gfromDocumentFields k x = case x of
    Right hm -> do
      field <- maybe (Left "no field defined for object shape") (Right . pack) k
      maybe (Left $ "missing field: " ++ unpack field) (fmap K1 . fromValue) (HM.lookup field hm)
    Left [v] -> K1 <$> fromValue v
    Left _   -> Left "unexpected multi-value record field"

instance GFromDocumentValue U1 where
  gfromDocumentFields (Just cname) (Left [value]) = do
    v <- fromValue value
    if (cname == v)
      then Right U1
      else Left $ "constructor mismatch: " ++ cname
  gfromDocumentFields _ _ = Right U1

instance GFromDocumentValue V1 where
  gfromDocumentFields _ _ = Left "can't construct generic type V1"

-- =============================================================
-- ================ Exposed ToDocValue class  ================== 
-- =============================================================
class ToDocValue a where
  toValue :: a -> FireStore.Value

  default toValue :: (Generic a, GToDocumentValue (Rep a)) => a -> FireStore.Value
  toValue = toArrayOrMapOrValue . gtoDocumentFields False Nothing . from

instance (ToDocValue a) => ToDocValue (Maybe a) where
  toValue (Just x) = toValue x
  toValue _        = FireStore.value & FireStore.vNullValue ?~ FireStore.NullValue

instance ToDocValue Text where
  toValue txt = FireStore.value & FireStore.vStringValue ?~ txt

instance ToDocValue String where
  toValue = toValue . pack

instance ToDocValue Double where
  toValue x = FireStore.value & FireStore.vDoubleValue ?~ x

instance ToDocValue Bool where
  toValue x = FireStore.value & FireStore.vBooleanValue ?~ x

instance ToDocValue Int where
  toValue x = FireStore.value & FireStore.vIntegerValue ?~ (fromInteger $ toInteger x)

instance (ToDocValue a, ToDocValue b) => ToDocValue (a, b) where
  toValue (a, b) = let
    arr = FireStore.arrayValue & FireStore.avValues .~ [toValue a, toValue b]
    in FireStore.value & FireStore.vArrayValue ?~ arr

instance (ToDocValue a, ToDocValue b, ToDocValue c) => ToDocValue (a, b, c) where
  toValue (a, b, c) = let
    arr = FireStore.arrayValue & FireStore.avValues .~ [toValue a, toValue b, toValue c]
    in FireStore.value & FireStore.vArrayValue ?~ arr

instance (ToDocValue a, ToDocValue b, ToDocValue c, ToDocValue d) => ToDocValue (a, b, c, d) where
  toValue (a, b, c, d) = let
    arr = FireStore.arrayValue & FireStore.avValues .~ [toValue a, toValue b, toValue c, toValue d]
    in FireStore.value & FireStore.vArrayValue ?~ arr

instance ToDocValue FireStore.DocumentFields where
  toValue docFields = let
    mv = FireStore.mapValueFields (docFields ^. FireStore.dfAddtional) 
    in FireStore.value & FireStore.vMapValue ?~ (FireStore.mapValue & FireStore.mvFields ?~ mv)

-- =============================================================
-- =========== Generic helper class ToDocValue ================= 
-- =============================================================

toArrayVaule :: [FireStore.Value] -> FireStore.Value
toArrayVaule vs = FireStore.value & FireStore.vArrayValue ?~ (FireStore.arrayValue & FireStore.avValues .~ vs)

fromArrayVaule :: FireStore.Value -> Maybe [FireStore.Value]
fromArrayVaule vl = do
  arr <- vl ^. FireStore.vArrayValue
  return $ arr ^. FireStore.avValues

toMapVaule :: HM.HashMap Text FireStore.Value -> FireStore.Value
toMapVaule vs = let
  mv = FireStore.mapValueFields vs
  in FireStore.value & FireStore.vMapValue ?~ (FireStore.mapValue & FireStore.mvFields ?~ mv)

fromMapVaule :: FireStore.Value -> Maybe (HM.HashMap Text FireStore.Value)
fromMapVaule vl = do
  mv <- vl ^. FireStore.vMapValue
  mf <- mv ^. FireStore.mvFields
  return $ mf ^. FireStore.mvfAddtional

toArrayOrMapOrValue :: ValueBuilder -> FireStore.Value
toArrayOrMapOrValue x = case x of
  Right fs -> toMapVaule fs
  Left [v] -> v
  Left  vs -> toArrayVaule vs

fromArrayOrMapOrValue :: FireStore.Value -> ValueBuilder
fromArrayOrMapOrValue v = case (fromMapVaule v, fromArrayVaule v) of
  (Just hm, Nothing)    -> Right hm
  (Nothing, Just arr)   -> Left arr
  _                     -> Left [v]

withTag :: String -> ValueBuilder -> ValueBuilder
withTag t v = let
  tag = toValue t
  in case v of
    Right hm -> Right $ HM.insert "__TAG__" tag hm  
    Left  [] -> Left  $ [tag]
    Left  vs -> Right $ HM.fromList [("__TAG__", tag), ("__VALUES__", toArrayVaule vs)]  

getTag :: String -> ValueBuilder -> Either String ValueBuilder
getTag tag v = case v of
  Right hm -> do
    tagValue <- maybe (Left "Object not tagged") fromValue $ HM.lookup "__TAG__" hm
    if (tagValue == tag) 
      then return $ maybe (Right hm) Left (HM.lookup "__VALUES__" hm >>= fromArrayVaule)
      else Left $ "tag mismatch: " ++ tagValue
  Left [x] -> do
    tagValue <- fromValue x
    if (tagValue == tag)
      then Right (Left [])
      else Left $ "tag mismatch: " ++ tagValue
  Left _ -> Left "can not get tag from array."

class GToDocumentValue f where
  gtoDocumentFields :: Bool -> Maybe String -> f a -> ValueBuilder

instance (GToDocumentValue a, GToDocumentValue b) => GToDocumentValue (a :*: b) where
  gtoDocumentFields hasTag k (x :*: y) = case (a, b) of
    (Right xs, Right ys) -> Right (HM.union xs ys)
    (Left  xs, Left  ys) -> Left  (xs ++ ys)
    _ -> error "Unexpected mix of record and positional fields"
    where
      a = gtoDocumentFields hasTag k x
      b = gtoDocumentFields hasTag k y

instance (GToDocumentValue a, GToDocumentValue b) => GToDocumentValue (a :+: b) where
  gtoDocumentFields _ _ (L1 x) = gtoDocumentFields True Nothing x
  gtoDocumentFields _ _ (R1 x) = gtoDocumentFields True Nothing x

instance (GToDocumentValue a, Datatype c) => GToDocumentValue (D1 c a) where
  gtoDocumentFields hasTag _ (M1 x) = gtoDocumentFields hasTag Nothing x

instance (GToDocumentValue a, Constructor c) => GToDocumentValue (C1 c a) where
  gtoDocumentFields hasTag _ m1@(M1 x)
    | conIsRecord m1 = maybeAddTag unwrap 
    | otherwise      = Left [toArrayOrMapOrValue . maybeAddTag $ unwrap]
    where
      maybeAddTag = if hasTag then withTag cname else id
      unwrap = gtoDocumentFields hasTag (Just cname) x
      cname = conName m1

instance (GToDocumentValue a, Selector c) => GToDocumentValue (S1 c a) where
  gtoDocumentFields hasTag _ m1@(M1 x)
      | fieldName == "" = gtoDocumentFields hasTag Nothing x
      | otherwise       = gtoDocumentFields hasTag (Just fieldName) x
      where fieldName = selName m1

instance (ToDocValue a) => GToDocumentValue (K1 i a) where
  gtoDocumentFields _ (Just k) (K1 x) = Right $ HM.singleton (pack k) (toValue x)
  gtoDocumentFields _ _        (K1 x) = Left [toValue x]

instance GToDocumentValue U1 where
  gtoDocumentFields False (Just v) _ = Left [toValue v]
  gtoDocumentFields _     _        _ = Left []

instance GToDocumentValue V1 where
  gtoDocumentFields _ _ _ = Left []




data Test deriving (Generic)
data Test0 = Test0A | Test0B | Test0C deriving (Generic, Eq, Show)
data Test1 = Test1A String String | Test1B | Test1C String deriving (Generic, Eq, Show)
data Test2 = Test2A {test2a :: Text, test2b :: Text} | Test2B deriving (Generic, Eq, Show)
data Test3 = Test3A {test3a :: Test2, test3b :: Test1} deriving (Generic, Eq, Show)
data Test4 = Test4A String String deriving (Generic, Eq, Show)

instance ToDocValue Test
instance ToDocValue Test0
instance ToDocValue Test1
instance ToDocValue Test2
instance ToDocValue Test3
instance ToDocValue Test4

instance FromDocValue Test
instance FromDocValue Test0
instance FromDocValue Test1
instance FromDocValue Test2
instance FromDocValue Test3
instance FromDocValue Test4

instance ToJSON Test
instance ToJSON Test0
instance ToJSON Test1
instance ToJSON Test2
instance ToJSON Test3
instance ToJSON Test4

matchStringValue :: FireStore.Value -> String -> Bool
matchStringValue v s = (v ^. FireStore.vStringValue) == (Just . pack $ s) 

matchArrayValue :: FireStore.Value -> [String] -> Bool
matchArrayValue v ss = all id $ zipWith matchStringValue vs ss
  where
    vs = v ^. FireStore.vArrayValue . _Just . FireStore.avValues

test = let
  [test4a, test4b] = (toValue $ Test4A "xx" "yy") ^. FireStore.vArrayValue . _Just . FireStore.avValues
  mapTest1a = (toValue $ Test1A "xx" "yy") ^. FireStore.vMapValue . _Just . FireStore.mvFields . _Just . FireStore.mvfAddtional
  mapTest2b = (toValue $ Test2A "xx" "yy") ^. FireStore.vMapValue . _Just . FireStore.mvFields . _Just . FireStore.mvfAddtional
  in [
    matchStringValue (toValue Test0A) (show Test0A),
    matchStringValue test4a "xx",
    matchStringValue test4b "yy",
    matchStringValue (mapTest1a HM.! "__TAG__") "Test1A",
    matchArrayValue  (mapTest1a HM.! "__VALUES__") ["xx", "yy"],
    matchStringValue (mapTest2b HM.! "test2a") "xx",
    matchStringValue (mapTest2b HM.! "test2b") "yy"
    ]

checkIso :: (ToDocValue a, FromDocValue a, Eq a) => a -> Bool
checkIso x = let
  regen = fromValue (toValue x)
  in case regen of
    Right v  -> v == x
    Left err -> error err

testIsomorphism = let
  test1a = Test1A "ss" "dd"
  test1b = Test1B
  test1c = Test1C "ss"
  test2a = Test2A "ss" "dd"
  in [
    checkIso test1a,
    checkIso test1b,
    checkIso test1c,
    checkIso test2a
    ]