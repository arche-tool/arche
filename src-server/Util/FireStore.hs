{-# LANGUAGE OverloadedStrings   #-}
module Util.FireStore where

import qualified Network.Google.FireStore as FireStore
import Control.Lens                 ((&), (^.), (^?), (?~), (.~), ix, _Just)
import Control.Monad                ((>=>))
import Data.Text                    (Text)
import Data.HashMap.Strict as HM

toDoc :: ToDocumentFields a => a -> FireStore.Document
toDoc x = FireStore.document & FireStore.dFields ?~ (toDocFields x)

fromDoc :: FromDocumentFields a => FireStore.Document -> Either String a
fromDoc = getDocumentFields >=> fromDocFields

class ToDocumentFields a where
    toDocFields :: a -> FireStore.DocumentFields

class FromDocumentFields a where
    fromDocFields :: FireStore.DocumentFields -> Either String a

getMaybeTextField :: FireStore.DocumentFields -> Text -> Either String (Maybe Text)
getMaybeTextField fields key = case fields ^. FireStore.dfAddtional ^? (ix key) of
    Nothing -> Right Nothing
    Just value -> maybe
        (Left $ "Field '" <> show(key) <> "' is not text.")
        (Right . Just)
        (value ^. FireStore.vStringValue)

getMaybeDoubleField :: FireStore.DocumentFields -> Text -> Either String (Maybe Double)
getMaybeDoubleField fields key = case fields ^. FireStore.dfAddtional ^? (ix key) of
    Nothing -> Right Nothing
    Just value -> maybe
        (Left $ "Field '" <> show(key) <> "' is not Double.")
        (Right . Just)
        (value ^. FireStore.vDoubleValue)

getTextField :: FireStore.DocumentFields -> Text -> Either String Text
getTextField fields key = do
    value <- maybe
        (Left $ "No field '" <> show(key) <> "' available.")
        Right
        (fields ^. FireStore.dfAddtional ^? (ix key))
    maybe
        (Left $ "Field '" <> show(key) <> "' is not Double.")
        Right
        (value ^. FireStore.vStringValue)

getDoubleField :: FireStore.DocumentFields -> Text -> Either String Double
getDoubleField fields key = do
    value <- maybe
        (Left $ "No field '" <> show(key) <> "' available.")
        Right
        (fields ^. FireStore.dfAddtional ^? (ix key))
    maybe
        (Left $ "Field '" <> show(key) <> "' is not Double.")
        Right
        (value ^. FireStore.vDoubleValue)

getNestedDocumentField :: FireStore.DocumentFields -> Text -> Either String FireStore.DocumentFields
getNestedDocumentField fields key = do
    value <- maybe
        (Left $ "No field '" <> show(key) <> "' available.")
        Right
        (fields ^. FireStore.dfAddtional ^? (ix key))
    maybe
        (Left $ "Field '" <> show(key) <> "' is not Double.")
        (Right . FireStore.documentFields)
        (value ^? FireStore.vMapValue . _Just . FireStore.mvFields . _Just . FireStore.mvfAddtional)

class ToDocValue a where
    toValueMaybe :: Maybe a -> FireStore.Value
    toValueMaybe (Just x) = toValue x
    toValueMaybe _        = FireStore.value

    toValue :: a -> FireStore.Value

class FromDocValue a where
    fromValue :: FireStore.Value -> Either String a

instance ToDocValue Text where
    toValueMaybe txt = FireStore.value & FireStore.vStringValue .~ txt
    toValue txt = FireStore.value & FireStore.vStringValue ?~ txt

instance ToDocValue FireStore.DocumentFields where
    toValue docFields = let
        mv = FireStore.mapValueFields (docFields ^. FireStore.dfAddtional) 
        in FireStore.value & FireStore.vMapValue ?~ (FireStore.mapValue & FireStore.mvFields ?~ mv)

buildDoc :: [(Text, FireStore.Value)] -> FireStore.Document
buildDoc ls = FireStore.document & FireStore.dFields ?~ (buildDocFields ls)

buildDocFields :: [(Text, FireStore.Value)] -> FireStore.DocumentFields
buildDocFields = FireStore.documentFields . HM.fromList

getDocumentFields :: FireStore.Document -> Either String FireStore.DocumentFields
getDocumentFields doc = maybe (Left "Empty document") Right (doc ^. FireStore.dFields)