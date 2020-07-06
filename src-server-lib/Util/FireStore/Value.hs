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
  , toMapValue
  ) where

import Control.Applicative ((<|>), liftA2, liftA3)
import Control.Lens        ((&), (?~), (.~), (^.))
import Data.Text           (Text, pack, unpack)
import GHC.Generics
import GHC.Base            (MonadPlus(..), Alternative(..))

import qualified Control.Monad.Fail       as MF
import qualified Data.HashMap.Strict      as HM
import qualified Network.Google.FireStore as FireStore

type ValueBuilder = Either [FireStore.Value] (HM.HashMap Text FireStore.Value)

-- =============================================================
-- ============== Exposed FromDocValue class  ================== 
-- =============================================================
class FromDocValue a where
  fromValue :: FireStore.Value -> Either String a

  default fromValue :: (Generic a, GFromDocumentValue (Rep a)) => FireStore.Value -> Either String a 
  fromValue = fmap to . gfromDocumentFields False Nothing . fromArrayOrMapOrValue 

instance FromDocValue Text where
  fromValue value = maybe (Left "not a string value") Right (value ^. FireStore.vStringValue)

instance FromDocValue String where
  fromValue = fmap unpack . fromValue

instance (FromDocValue a) => FromDocValue (Maybe a) where
  fromValue value = case value ^. FireStore.vNullValue of
    Just nullVal
     | nullVal == FireStore.NullValue -> Right Nothing
     | otherwise                      -> Left "unexpected null value"
    Nothing -> Just <$> fromValue value

instance FromDocValue Double where
  fromValue value = maybe (Left "not a double value") Right (value ^. FireStore.vDoubleValue)

instance FromDocValue Bool where
  fromValue value = maybe (Left "not a boolean value") Right (value ^. FireStore.vBooleanValue)

instance FromDocValue Int where
  fromValue value = maybe (Left "not a integer value") (Right . fromEnum) (value ^. FireStore.vIntegerValue)

instance (FromDocValue a, FromDocValue b) => FromDocValue (a, b) where
  fromValue value = do
    arr <- maybe (Left "not a tuple") Right (value ^. FireStore.vArrayValue)
    case arr ^. FireStore.avValues of
      [a,b] -> liftA2 (,) (fromValue a) (fromValue b)
      _     -> Left "Unexpected number of tuple elements." 

instance (FromDocValue a, FromDocValue b, FromDocValue c) => FromDocValue (a, b, c) where
  fromValue value = do
    arr <- maybe (Left "not a tuple") Right (value ^. FireStore.vArrayValue)
    case arr ^. FireStore.avValues of
      [a,b,c] -> liftA3 (,,) (fromValue a) (fromValue b) (fromValue c)
      _       -> Left "Unexpected number of tuple elements." 

instance (FromDocValue a, FromDocValue b, FromDocValue c, FromDocValue d) => FromDocValue (a, b, c, d) where
  fromValue value = do
    arr <- maybe (Left "not a tuple") Right (value ^. FireStore.vArrayValue)
    case arr ^. FireStore.avValues of
      [a,b,c,d] -> do
        va <- fromValue a
        vb <- fromValue b
        vc <- fromValue c
        vd <- fromValue d
        return (va, vb, vc, vd)
      _         -> Left "Unexpected number of tuple elements." 

instance FromDocValue FireStore.DocumentFields where
  fromValue = maybe
    (Left "Value is not a map")
    (Right . FireStore.documentFields) . fromMapValue

-- =============================================================
-- =========== Generic helper class FromDocValue ================= 
-- =============================================================

class GFromDocumentValue f where
  gfromDocumentFields :: Bool -> Maybe String -> ValueBuilder -> Either String (f a)

instance (GFromDocumentValue a, GFromDocumentValue b) => GFromDocumentValue (a :*: b) where
  gfromDocumentFields hasTag k x = case x of
    Left (v:vs) -> do
      a <- gfromDocumentFields hasTag k (Left [v])
      b <- gfromDocumentFields hasTag k (Left vs)
      return (a :*: b)
    hm -> do
      a <- gfromDocumentFields hasTag k hm
      b <- gfromDocumentFields hasTag k hm
      return (a :*: b)

instance (GFromDocumentValue a, GFromDocumentValue b) => GFromDocumentValue (a :+: b) where
  gfromDocumentFields _ k x = 
      (R1 <$> gfromDocumentFields True k x) <|> (L1 <$>gfromDocumentFields True k x) 

instance (GFromDocumentValue a, Datatype c) => GFromDocumentValue (D1 c a) where
  gfromDocumentFields hasTag k x = M1 <$> (gfromDocumentFields hasTag k x)

instance (GFromDocumentValue a, Constructor c) => GFromDocumentValue (C1 c a) where
  gfromDocumentFields hasTag _ x = M1 <$> case hasTag of
    True -> getTag cname x >>= gfromDocumentFields False Nothing
    _    -> gfromDocumentFields False Nothing x
    where
      cname = conName (undefined :: M1 _i c _f _p)

instance (GFromDocumentValue a, Selector c) => GFromDocumentValue (S1 c a) where
  gfromDocumentFields hasTag _ x = M1 <$> case x of
    Right hm -> maybe 
      (Left $ "missing field: " ++ fieldName)
      (gfromDocumentFields hasTag Nothing . fromArrayOrMapOrValue)
      (HM.lookup (pack fieldName) hm)
    Left [v] -> gfromDocumentFields hasTag (Just fieldName) (fromArrayOrMapOrValue v)
    Left _   -> Left "unexpected multi-value record field"
    where fieldName = selName (undefined :: M1 _i c _f _p)

instance (FromDocValue a) => GFromDocumentValue (K1 i a) where
  gfromDocumentFields _ k x = K1 <$> case x of
    Right hm -> case k of
      Just field -> maybe (Left $ "missing field: " ++ field) fromValue (HM.lookup (pack field) hm)
      _          -> fromValue (toMapValue hm) 
    Left [v] -> fromValue v
    Left _   -> Left "unexpected multi-value record field"

instance GFromDocumentValue U1 where
  gfromDocumentFields _ (Just cname) (Left [value]) = do
    v <- fromValue value
    if (cname == v)
      then Right U1
      else Left $ "constructor mismatch: " ++ cname
  gfromDocumentFields _ _ _ = Right U1

instance GFromDocumentValue V1 where
  gfromDocumentFields _ _ _ = Left "can't construct generic type V1"

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
  toValue x = FireStore.value & FireStore.vIntegerValue ?~ (toEnum x)

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

toMapValue :: HM.HashMap Text FireStore.Value -> FireStore.Value
toMapValue vs = let
  mv = FireStore.mapValueFields vs
  in FireStore.value & FireStore.vMapValue ?~ (FireStore.mapValue & FireStore.mvFields ?~ mv)

fromMapValue :: FireStore.Value -> Maybe (HM.HashMap Text FireStore.Value)
fromMapValue vl = do
  mv <- vl ^. FireStore.vMapValue
  mf <- mv ^. FireStore.mvFields
  return $ mf ^. FireStore.mvfAddtional

toArrayOrMapOrValue :: ValueBuilder -> FireStore.Value
toArrayOrMapOrValue x = case x of
  Right fs -> toMapValue fs
  Left [v] -> v
  Left  vs -> toArrayVaule vs

fromArrayOrMapOrValue :: FireStore.Value -> ValueBuilder
fromArrayOrMapOrValue v = case (fromMapValue v, fromArrayVaule v) of
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


-- =========================== Parser =======================

newtype Parser a = P (FireStore.Value -> Either String (a, FireStore.Value))

instance Functor Parser where
  fmap h (P f) = P $ \k -> case (f k) of
    Right (x, v) -> Right (h x, v)
    Left err     -> Left err

instance Applicative Parser where
  pure x = P (\k -> pure (x, k))
  (<*>) (P fab) (P pa) = P $ \k -> do
    (f, v) <- fab k
    (a, t) <- pa v
    pure (f a, t) 

instance Monad Parser where
  fail e    = P (\_ -> Left e)
  P m >>= f = P $ \k -> case m k of
    Right (x, v) -> let P h = (f x) in h v
    Left err     -> Left err 

instance MF.MonadFail Parser where
  fail e = P (\_ -> Left e)

instance Alternative Parser where
  empty = P (\_ -> Left "Empty parser")
  (<|>) (P a) (P b) = P $ \k -> case a k of
    Left _ -> b k
    done   -> done 

instance MonadPlus Parser

parseEnd :: Parser FireStore.Value
parseEnd = P $ \value -> pure (value, value)

runParser :: Parser a -> FireStore.Value -> Either String a
runParser (P parser) value = fst <$> parser value

genRawParser :: (FireStore.Value -> Maybe a) -> String -> FireStore.Value -> Parser a
genRawParser foo errMsg = \value -> case foo value of
  Just v -> pure v
  _      -> fail errMsg

genRawMaybeParser :: (FireStore.Value -> Maybe a) -> FireStore.Value -> Parser (Maybe a)
genRawMaybeParser foo = \value -> case foo value of
  Just v -> pure (Just v)
  _      -> pure Nothing


optional :: Parser a -> Parser (Maybe a)
optional innerParser = do
  maybeNull <- parseEnd >>= genRawMaybeParser (^. FireStore.vNullValue)
  case maybeNull of
    Just nullVal
      | nullVal == FireStore.NullValue -> pure Nothing
      | otherwise                      -> fail "unexpected null value"
    Nothing -> (fmap Just innerParser)

class FromValue2 a where
  fromValue2 :: FireStore.Value -> Parser a

instance FromValue2 Text where
  fromValue2 = genRawParser (^. FireStore.vStringValue) "not a string value"

instance FromValue2 String where
  fromValue2 = fmap unpack . fromValue2

instance FromValue2 Double where
  fromValue2 = genRawParser (^. FireStore.vDoubleValue) "not a double value"

instance FromValue2 Bool where
  fromValue2 = genRawParser (^. FireStore.vBooleanValue) "not a boolean value"

instance FromValue2 Int where
  fromValue2 = genRawParser (fmap fromEnum . (^. FireStore.vIntegerValue)) "not a integer value"

instance FromValue2 (HM.HashMap Text FireStore.Value) where
  fromValue2 = genRawParser fromMapValue "Value is not a map"

instance FromValue2 [FireStore.Value] where
  fromValue2 = genRawParser fromArrayVaule "Value is not an array"

instance {-# OVERLAPPABLE #-} (FromValue2 a) => FromValue2 [a] where
  fromValue2 v = do
    vs <- fromValue2 v
    mapM fromValue2 vs

parse :: (FromValue2 a) => Parser a
parse = parseEnd >>= fromValue2 

withObject :: (HM.HashMap Text FireStore.Value -> Parser a) -> Parser a
withObject foo = parseEnd >>= fromValue2 >>= foo

(.:) :: (FromValue2 a) => HM.HashMap Text FireStore.Value -> Text -> Parser a
(.:) hm key = case HM.lookup key hm of
  Just v -> fromValue2 v
  _      -> fail $ "Object does not contain key \"" ++ unpack key ++ "\""

-- =========================== Parser =======================
class ToValue2 a where
  toValue2 :: a -> FireStore.Value

instance (ToValue2 a) => ToValue2 (Maybe a) where
  toValue2 (Just x) = toValue2 x
  toValue2 _        = FireStore.value & FireStore.vNullValue ?~ FireStore.NullValue

instance ToValue2 Text where
  toValue2 txt = FireStore.value & FireStore.vStringValue ?~ txt

instance ToValue2 String where
  toValue2 = toValue2 . pack

instance ToValue2 Double where
  toValue2 x = FireStore.value & FireStore.vDoubleValue ?~ x

instance ToValue2 Bool where
  toValue2 x = FireStore.value & FireStore.vBooleanValue ?~ x

instance ToValue2 Int where
  toValue2 x = FireStore.value & FireStore.vIntegerValue ?~ (toEnum x)
  
instance ToValue2 [FireStore.Value] where
  toValue2 vs = FireStore.value & FireStore.vArrayValue ?~ (FireStore.arrayValue & FireStore.avValues .~ vs)

instance {-# OVERLAPPABLE #-} (ToValue2 a) => ToValue2 [a] where
  toValue2 = toValue2 . map toValue2 

instance ToValue2 (HM.HashMap Text FireStore.Value) where
  toValue2 vs = let
    mv = FireStore.mapValueFields vs
    in FireStore.value & FireStore.vMapValue ?~ (FireStore.mapValue & FireStore.mvFields ?~ mv)

instance {-# OVERLAPPABLE #-} (ToValue2 a) => ToValue2 (HM.HashMap Text a) where
  toValue2 = toValue2 . HM.map toValue2  

instance (ToValue2 a, ToValue2 b) => ToValue2 (a, b) where
  toValue2 (a, b) = toValue2 [toValue2 a, toValue2 b]

instance (ToValue2 a, ToValue2 b, ToValue2 c) => ToValue2 (a, b, c) where
  toValue2 (a, b, c) = toValue2 [toValue2 a, toValue2 b, toValue2 c]

instance (ToValue2 a, ToValue2 b, ToValue2 c, ToValue2 d) => ToValue2 (a, b, c, d) where
  toValue2 (a, b, c, d) = toValue2 [toValue2 a, toValue2 b, toValue2 c, toValue2 d]

instance ToValue2 FireStore.DocumentFields where
  toValue2 docFields = let
    mv = FireStore.mapValueFields (docFields ^. FireStore.dfAddtional) 
    in FireStore.value & FireStore.vMapValue ?~ (FireStore.mapValue & FireStore.mvFields ?~ mv)

-- === test
data T = T Int String deriving Show

instance ToValue2 T where
  toValue2 (T a b) = toValue2 [toValue2 a, toValue2 b]

pp = do
  [a, b] <- parse
  T <$> fromValue2 a <*> fromValue2 b 
test :: Parser (Int, String)
test = withObject $ \v -> (,)
        <$> v .: "x"
        <*> v .: "y"
