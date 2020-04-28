{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
module Util.FireStore.Document where

import Control.Lens                 ((&), (^.), (^?), (?~), ix, _Just)
import Data.Text                    (Text)

import qualified Data.HashMap.Strict      as HM
import qualified Network.Google.FireStore as FireStore

import Util.FireStore.Value ()

-- ============== Document store ================
buildDocFromValue :: FireStore.Value -> FireStore.Document
buildDocFromValue v = case mapValue of
    Just m -> let
        hm = m ^. FireStore.mvFields . _Just . FireStore.mvfAddtional
        df = FireStore.documentFields hm
        in FireStore.document & FireStore.dFields ?~ df
    _ -> buildDoc [("value", v)]
    where
        mapValue = v ^. FireStore.vMapValue 

buildDoc :: [(Text, FireStore.Value)] -> FireStore.Document
buildDoc ls = FireStore.document & FireStore.dFields ?~ (buildDocFields ls)

buildDocFields :: [(Text, FireStore.Value)] -> FireStore.DocumentFields
buildDocFields = FireStore.documentFields . HM.fromList

getDocumentFields :: FireStore.Document -> Either String FireStore.DocumentFields
getDocumentFields doc = maybe (Left "Empty document") Right (doc ^. FireStore.dFields)

-- TODO: Remove after Generic
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