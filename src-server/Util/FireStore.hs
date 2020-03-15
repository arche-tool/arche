{-# LANGUAGE OverloadedStrings   #-}
module Util.FireStore where

import qualified Network.Google.FireStore as FireStore
import Control.Lens                 ((&), (^.), (^?), (?~), (.~), ix, set)
import Data.Text                    (Text)
import Data.HashMap.Strict as HM

class ToDocument a where
    toDoc :: a -> FireStore.Document

class FromDocument a where
    fromDoc :: FireStore.Document -> Either String a

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


class ToDocValue a where
    toValueMaybe :: Maybe a -> FireStore.Value

    toValue :: a -> FireStore.Value
    toValue = toValueMaybe . Just

instance ToDocValue Text where
    toValueMaybe txt = FireStore.value & FireStore.vStringValue .~ txt
    toValue txt = FireStore.value & FireStore.vStringValue ?~ txt

buildDoc :: [(Text, FireStore.Value)] -> FireStore.Document
buildDoc ls = let 
    hmap = HM.fromList ls
    in FireStore.document & FireStore.dFields ?~ (FireStore.documentFields hmap)

getDocumentFields :: FireStore.Document -> Either String FireStore.DocumentFields
getDocumentFields doc = maybe (Left "Empty document") Right (doc ^. FireStore.dFields)