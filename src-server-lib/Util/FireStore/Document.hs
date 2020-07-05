{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
module Util.FireStore.Document where

import Control.Lens                 ((&), (^.), (?~), _Just)
import Data.Text                    (Text)

import qualified Data.HashMap.Strict      as HM
import qualified Network.Google.FireStore as FireStore

import Util.FireStore.Value (toMapValue)

-- ============== Document store ================
singleDocInternalKey :: Text
singleDocInternalKey = "__DOC__VALUE__"

buildDocFromValue :: FireStore.Value -> FireStore.Document
buildDocFromValue v = case mapValue of
    Just m -> let
        hm = m ^. FireStore.mvFields . _Just . FireStore.mvfAddtional
        df = FireStore.documentFields hm
        in FireStore.document & FireStore.dFields ?~ df
    _ -> buildDoc [(singleDocInternalKey, v)]
    where
        mapValue = v ^. FireStore.vMapValue 

buildDoc :: [(Text, FireStore.Value)] -> FireStore.Document
buildDoc ls = FireStore.document & FireStore.dFields ?~ (buildDocFields ls)

buildDocFields :: [(Text, FireStore.Value)] -> FireStore.DocumentFields
buildDocFields = FireStore.documentFields . HM.fromList

getDocumentFields :: FireStore.Document -> Either String FireStore.DocumentFields
getDocumentFields doc = maybe (Left "Empty document") Right (doc ^. FireStore.dFields)

buildValueFromDoc :: FireStore.Document -> Either String FireStore.Value
buildValueFromDoc doc = do
    fields <- getDocumentFields doc
    let hm = fields ^. FireStore.dfAddtional
    case HM.lookup singleDocInternalKey hm of
        Just v -> return v
        _      ->  return (toMapValue hm) 
