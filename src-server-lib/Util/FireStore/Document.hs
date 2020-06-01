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

buildValueFromDoc :: FireStore.Document -> Either String FireStore.Value
buildValueFromDoc doc = do
    fields <- getDocumentFields doc
    return . toMapValue $ fields ^. FireStore.dfAddtional
