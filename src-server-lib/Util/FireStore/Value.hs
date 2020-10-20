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
  -- Parse Value
  , Parser
  , parse
  , runParser
  , optional
  , preserveContext
  , setParserContext
  , (.:)
  , parseArrayHeadWith
  ) where

import Control.Applicative ((<|>))
import Control.Lens        ((&), (?~), (.~), (^.), (^?), _Just)
import Data.Bits           (unsafeShiftR)
import Data.Text           (Text, pack, unpack)
import GHC.Generics
import GHC.Base            (MonadPlus(..), Alternative(..))

import qualified Control.Monad.Fail       as MF
import qualified Data.HashMap.Strict      as HM
import qualified Network.Google.FireStore as FireStore

-- |==Type===|====Description ========================
-- V1        | lifted version of Empty
-- U1        | lifted version of ()
-- (:+:)     | lifted version of Either
-- (:*:)     | lifted version of (,)
-- D1 = M1 D | meta-information for datatypes
-- C1 = M1 C | meta-information for constructors
-- S1 = M1 S | meta-information for record selectors
-- K1        | concrete type
-- ===================================================

-- ========================== Pseudo-code examples ==================================
-- >> data T = T Int Int -> [int, int]
-- >> data T = T {aaa: Int, bbb: Int} -> {aaa: int, bbb: int}
-- >> 
-- >> data T = T0 Int Int | T1 String String -> {"T1": [int, int]}
-- >> data T = T0 Int Int | T1 {aaa: Int, bbb: Int} -> {"T1": {aaa: int, bbb: int}}
-- ===================================================================================

-- =============================================================
-- ============== Exposed FromDocValue class  ================== 
-- =============================================================
class FromDocValue a where
  fromValue :: FireStore.Value -> Either String a

  default fromValue :: (Generic a, GFromDocumentValue (Rep a)) => FireStore.Value -> Either String a 
  fromValue = fmap to . runParser (gfromDocumentFields False Nothing) 

-- =============================================================
-- =========== Generic helper class FromDocValue =============== 
-- =============================================================
class GFromDocumentValue f where
  gfromDocumentFields :: Bool -> Maybe String -> Parser (f a)

instance (GFromDocumentValue a, GFromDocumentValue b) => GFromDocumentValue (a :*: b) where
  gfromDocumentFields hasTag k = do
    es :: Either [FireStore.Value] (HM.HashMap Text FireStore.Value) <- (Right <$> parse) <|> (Left <$> parse)
    case es of
      Left vs -> let 
        len  = length vs
        lenL = len `unsafeShiftR` 1
        rewrap ls = case ls of
          [x] -> x
          xs  -> toArrayVaule xs
        in do
          a <- setParserContext (rewrap $ take lenL vs) >> gfromDocumentFields hasTag k 
          b <- setParserContext (rewrap $ drop lenL vs) >> gfromDocumentFields hasTag k 
          return (a :*: b)
      Right hm -> do
        let rehm = toMapValue hm
        a <- setParserContext rehm >> gfromDocumentFields hasTag k 
        b <- setParserContext rehm >> gfromDocumentFields hasTag k 
        return (a :*: b)

instance (GFromDocumentValue a, GFromDocumentValue b) => GFromDocumentValue (a :+: b) where
  gfromDocumentFields _ k = 
      (R1 <$> gfromDocumentFields True k) <|> (L1 <$>gfromDocumentFields True k) 

instance (GFromDocumentValue a, Datatype c) => GFromDocumentValue (D1 c a) where
  gfromDocumentFields hasTag k = M1 <$> (gfromDocumentFields hasTag k)

instance (GFromDocumentValue a, Constructor c) => GFromDocumentValue (C1 c a) where
  gfromDocumentFields hasTag _ = M1 <$> case hasTag of
    True -> getTag cname >>= setParserContext >> gfromDocumentFields False Nothing
    _    -> gfromDocumentFields False Nothing
    where
      cname = conName (undefined :: M1 _i c _f _p)

instance (GFromDocumentValue a, Selector c) => GFromDocumentValue (S1 c a) where
  gfromDocumentFields hasTag _ = M1 <$> case fieldName of
    ""  -> gfromDocumentFields hasTag Nothing
    tag -> getTag tag >>= setParserContext >> gfromDocumentFields hasTag (Just fieldName)
    where fieldName = selName (undefined :: M1 _i c _f _p)

instance (FromDocValue a) => GFromDocumentValue (K1 i a) where
  gfromDocumentFields _ _ = K1 <$> parse

instance GFromDocumentValue U1 where
  gfromDocumentFields _ (Just cname) = do
    v <- parse
    if (cname == v)
      then pure U1
      else fail $ "constructor mismatch: " ++ cname
  gfromDocumentFields _ _ = pure U1

instance GFromDocumentValue V1 where
  gfromDocumentFields _ _ = fail "can't construct generic type V1"

-- =============================================================
-- ================ Exposed ToDocValue class  ================== 
-- =============================================================
class ToDocValue a where
  toValue :: a -> FireStore.Value

  default toValue :: (Generic a, GToDocumentValue (Rep a)) => a -> FireStore.Value
  toValue = toArrayOrMapOrValue . gtoDocumentFields False Nothing . from

-- =============================================================
-- =========== Generic helper class ToDocValue ================= 
-- =============================================================

toArrayVaule :: [FireStore.Value] -> FireStore.Value
toArrayVaule vs = FireStore.value & FireStore.vArrayValue ?~ (FireStore.arrayValue & FireStore.avValues .~ vs)

fromArrayVaule :: FireStore.Value -> Maybe [FireStore.Value]
fromArrayVaule vl = vl ^? FireStore.vArrayValue . _Just . FireStore.avValues

toMapValue :: HM.HashMap Text FireStore.Value -> FireStore.Value
toMapValue vs = let
  mv = FireStore.mapValueFields vs
  in FireStore.value & FireStore.vMapValue ?~ (FireStore.mapValue & FireStore.mvFields ?~ mv)

fromMapValue :: FireStore.Value -> Maybe (HM.HashMap Text FireStore.Value)
fromMapValue vl = vl ^? FireStore.vMapValue . _Just . FireStore.mvFields . _Just . FireStore.mvfAddtional

toArrayOrMapOrValue :: ValueBuilder -> FireStore.Value
toArrayOrMapOrValue x = case x of
  Right fs -> toMapValue (HM.fromList fs)
  Left [v] -> v
  Left  vs -> toArrayVaule vs

getTag :: String -> Parser FireStore.Value
getTag tag = do
  hm <- parse
  maybe (fail $ "Could not find tag " ++ tag) pure . HM.lookup (pack tag) $ hm

type ValueBuilder = Either [FireStore.Value] [(Text, FireStore.Value)]

class GToDocumentValue f where
  gtoDocumentFields :: Bool -> Maybe String -> f a -> ValueBuilder

instance (GToDocumentValue a, GToDocumentValue b) => GToDocumentValue (a :*: b) where
  gtoDocumentFields hasTag k (x :*: y) = case (a, b) of
    (Right xs, Right ys) -> Right $ xs ++ ys
    (Left  xs, Left  ys) -> Left  $ xs ++ ys
    _                    -> error "Oops! Did not expected tagged/untagged to coexist."
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
    | hasTag    = Right [(pack cname, toArrayOrMapOrValue unwrap)]
    | otherwise = unwrap
    where
      unwrap = gtoDocumentFields hasTag (Just cname) x
      cname = conName m1

instance (GToDocumentValue a, Selector c) => GToDocumentValue (S1 c a) where
  gtoDocumentFields hasTag _ m1@(M1 x)
      | fieldName == "" = gtoDocumentFields hasTag Nothing x
      | otherwise       = gtoDocumentFields hasTag (Just fieldName) x
      where fieldName = selName m1

instance (ToDocValue a) => GToDocumentValue (K1 i a) where
  gtoDocumentFields _ (Just k) (K1 x) = Right [(pack k, toValue x)]
  gtoDocumentFields _ _        (K1 x) = Left [toValue x]

instance GToDocumentValue U1 where
  gtoDocumentFields _ (Just v) _ = Left [toValue v]
  gtoDocumentFields _ _        _ = Left []

instance GToDocumentValue V1 where
  gtoDocumentFields _ _ _ = Left []

-- =========================== Parser =======================

newtype Parser a = P (Maybe FireStore.Value -> Either String (a, Maybe FireStore.Value))

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

failNoContext :: (Monad m) => m a 
failNoContext = fail "No context left, it was consumed."

runParser :: Parser a -> FireStore.Value -> Either String a
runParser p = fmap fst . evalParser p

evalParser :: Parser a -> FireStore.Value -> Either String (a, Maybe FireStore.Value)
evalParser (P p) = p . Just

eitherFailPure :: Either String a -> Parser a
eitherFailPure = either fail pure

optional :: Parser a -> Parser (Maybe a)
optional innerParser = do
  value <- parse
  case value ^. FireStore.vNullValue of
    Just nullVal
      | nullVal == FireStore.NullValue -> pure Nothing
      | otherwise                      -> fail "unexpected null value"
    Nothing
      -- In reality a value with all fields empty is also treated as null/nothing
      | isItAllNull value -> pure Nothing
      -- Put context back for inner parser
      | otherwise         -> setParserContext value >> fmap Just innerParser

isItAllNull :: FireStore.Value -> Bool
isItAllNull v = not $ any id
  [ hasSomething FireStore.vGeoPointValue
  , hasSomething FireStore.vBytesValue
  , hasSomething FireStore.vIntegerValue
  , hasSomething FireStore.vTimestampValue
  , hasSomething FireStore.vDoubleValue
  , hasSomething FireStore.vStringValue
  , hasSomething FireStore.vBooleanValue
  , hasSomething FireStore.vMapValue
  , hasSomething FireStore.vArrayValue
  , hasSomething FireStore.vReferenceValue
  , hasSomething FireStore.vNullValue
  ]
  where
    hasSomething f = maybe False (const True) (v ^. f) 

parse :: (FromDocValue a) => Parser a
parse = P $ \v -> fmap (\a -> (a, Nothing)) (maybe failNoContext fromValue v)

(.:) :: (FromDocValue a) => HM.HashMap Text FireStore.Value -> Text -> Parser a
(.:) hm key = case HM.lookup key hm of
  Just v -> eitherFailPure $ fromValue v
  _      -> fail $ "Object does not contain key \"" ++ unpack key ++ "\""

-- Needs to import Debug.Trace
--traceContext :: Parser ()
--traceContext = P $ \v -> pure ((), trace (show v) v)

preserveContext :: Parser a -> Parser a
preserveContext (P p) = P $ \c -> case p c of
  Right (x, _) -> Right (x, c)
  Left err     -> Left err

setParserContext :: FireStore.Value -> Parser ()
setParserContext v = P $ \_ -> pure ((), Just v)

parseArrayHeadWith :: Parser a -> Parser a
parseArrayHeadWith p = do
  xs <- parse
  case xs of
    (v:vs) -> case runParser p v of
      Left err -> fail err
      Right a  -> setParserContext (toArrayVaule vs) >> pure a
    _     -> fail "Empty array"

-- ========================= FromValue Base Instances =======================

genRawParser :: (FireStore.Value -> Maybe a) -> String -> FireStore.Value -> Either String a
genRawParser foo errMsg = \value -> case foo value of
  Just v -> Right v
  _      -> Left errMsg

instance FromDocValue FireStore.Value where
  fromValue = pure

instance FromDocValue Text where
  fromValue = genRawParser (^. FireStore.vStringValue) "not a string value"

instance FromDocValue String where
  fromValue = fmap unpack . fromValue

instance FromDocValue Double where
  fromValue = genRawParser (^. FireStore.vDoubleValue) "not a double value"

instance FromDocValue Bool where
  fromValue = genRawParser (^. FireStore.vBooleanValue) "not a boolean value"

instance FromDocValue Int where
  fromValue = genRawParser (fmap fromEnum . (^. FireStore.vIntegerValue)) "not a integer value"

instance {-# OVERLAPPABLE #-} (Enum a) => FromDocValue a where
  fromValue = fmap toEnum . fromValue 

instance FromDocValue (HM.HashMap Text FireStore.Value) where
  fromValue = genRawParser fromMapValue "Value is not a map"

instance FromDocValue [FireStore.Value] where
  fromValue = genRawParser fromArrayVaule "Value is not an array"

instance {-# OVERLAPPABLE #-} (FromDocValue a) => FromDocValue [a] where
  fromValue v = do
    vs <- fromValue v
    mapM fromValue vs

instance (FromDocValue a, FromDocValue b) => FromDocValue (a, b) where
  fromValue v = case fromValue v of
    Right [a, b] -> (,) <$> fromValue a <*> fromValue b
    Right _      -> Left "Unexpected number of values in the tuple."
    Left err     -> Left err

instance (FromDocValue a, FromDocValue b, FromDocValue c) => FromDocValue (a, b, c) where
  fromValue v = case fromValue v of
    Right [a, b, c] -> (,,) <$> fromValue a <*> fromValue b <*> fromValue c
    Right _         -> Left "Unexpected number of values in the tuple."
    Left err        -> Left err

instance (FromDocValue a, FromDocValue b, FromDocValue c, FromDocValue d) => 
  FromDocValue (a, b, c, d) where
  fromValue v = case fromValue v of
    Right [a, b, c, d] -> (,,,) <$> fromValue a <*> fromValue b <*> fromValue c <*> fromValue d
    Right _            -> Left "Unexpected number of values in the tuple."
    Left err           -> Left err

instance (FromDocValue a) => FromDocValue (Maybe a) where
  fromValue = runParser (optional parse)

instance (FromDocValue a, FromDocValue b) => FromDocValue (Either a b) where
  fromValue v = do
    hm :: HM.HashMap Text FireStore.Value <- runParser parse v
    case (HM.lookup "Left" hm, HM.lookup "Right" hm) of
      (Just x, _) -> Left  <$> fromValue x
      (_, Just x) -> Right <$> fromValue x
      _           -> fail "Can not find neither Left nor Right" 

-- ========================= ToDocValue Base Instances =======================
instance (ToDocValue a) => ToDocValue (Maybe a) where
  toValue (Just x) = toValue x
  toValue _        = FireStore.value & FireStore.vNullValue ?~ FireStore.NullValue

instance (ToDocValue a, ToDocValue b) => ToDocValue (Either a b) where
  toValue (Left  a) = toValue $ HM.singleton ("Left"  :: Text) (toValue a)
  toValue (Right b) = toValue $ HM.singleton ("Right" :: Text) (toValue b)

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
  
instance {-# OVERLAPPABLE #-} (Enum a) => ToDocValue a where
  toValue = toValue . fromEnum

instance ToDocValue [FireStore.Value] where
  toValue vs = FireStore.value & FireStore.vArrayValue ?~ (FireStore.arrayValue & FireStore.avValues .~ vs)

instance {-# OVERLAPPABLE #-} (ToDocValue a) => ToDocValue [a] where
  toValue = toValue . map toValue 

instance ToDocValue (HM.HashMap Text FireStore.Value) where
  toValue vs = let
    mv = FireStore.mapValueFields vs
    in FireStore.value & FireStore.vMapValue ?~ (FireStore.mapValue & FireStore.mvFields ?~ mv)

instance {-# OVERLAPPABLE #-} (ToDocValue a) => ToDocValue (HM.HashMap Text a) where
  toValue = toValue . HM.map toValue  

instance (ToDocValue a, ToDocValue b) => ToDocValue (a, b) where
  toValue (a, b) = toValue [toValue a, toValue b]

instance (ToDocValue a, ToDocValue b, ToDocValue c) => ToDocValue (a, b, c) where
  toValue (a, b, c) = toValue [toValue a, toValue b, toValue c]

instance (ToDocValue a, ToDocValue b, ToDocValue c, ToDocValue d) => ToDocValue (a, b, c, d) where
  toValue (a, b, c, d) = toValue [toValue a, toValue b, toValue c, toValue d]

instance ToDocValue FireStore.DocumentFields where
  toValue docFields = let
    mv = FireStore.mapValueFields (docFields ^. FireStore.dfAddtional) 
    in FireStore.value & FireStore.vMapValue ?~ (FireStore.mapValue & FireStore.mvFields ?~ mv)