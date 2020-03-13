{-# LANGUAGE OverloadedStrings   #-}
module Type.Store where

import qualified Network.Google.FireStore as FireStore
import Control.Lens                 ((&), (.~), (^.), (^?), (<&>), (?~), (^?!), ix, preview, _Just)
import Data.Text                    (Text)

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

-- ================ User ================
data User
    = User
    { email :: Text
    , name :: Maybe Text
    } deriving (Show)

instance FromDocument User where
    fromDoc doc = do
        fields <- maybe (Left "Empty document") Right (doc ^. FireStore.dFields)
        _email <- getTextField fields "email"
        _name  <- getMaybeTextField fields "name"
        return $ User
            { email = _email
            , name = _name
            }

