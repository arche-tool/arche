{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.App where

import Servant
import Control.Monad                 (forM_)
import Data.String                   (fromString)
import Data.Text                     (Text)
import Servant.HTML.Blaze            (HTML)
import Text.Blaze.Html5              (Html, (!))

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import Web.Config (ElmAssets(..), getStaticAsset)

index :: ElmAssets -> Html
index elmAssets = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title "Arche"
        forM_ manifest $ \a -> H.link ! A.rel "manifest" ! A.href a
        forM_ favicon $ \a -> H.link ! A.rel "shortcut icon" ! A.href a
        H.link ! A.rel "stylesheet" ! A.href css
    H.body $ do
        H.noscript "You need to enable JavaScript to run this app."
        H.div mempty ! A.id "root"
        H.script mempty ! A.src runtime
        H.script mempty ! A.src vendor
        H.script mempty ! A.src main
    where
        runtime = fromString (runtimeJs elmAssets)
        vendor = fromString (vendorJs elmAssets)
        main = fromString (mainJs elmAssets)
        css = fromString (mainCss elmAssets)
        manifest = fromString <$> getStaticAsset elmAssets "static/media/manifest.json"
        favicon = fromString <$> getStaticAsset elmAssets "static/media/favicon.ico"

type Url  = Text

type App = (Capture "url" Url :> Get '[HTML] Html) :<|> "static" :> Raw

appServer :: FilePath -> ElmAssets -> Server App
appServer static elmAssets = homepage elmAssets :<|> serveDirectoryWebApp static

homepage :: ElmAssets -> Url -> Handler Html
homepage elmAssets url = let
    siglePage = pure (index elmAssets)
    throw404 = throwError $ err404 {
        errBody = "I still haven't found what I'm looking for ... O_o"
        } 
    in case url of
        ""                -> siglePage
        "index.html"      -> siglePage
        "upload"          -> siglePage
        "reconstructions" -> siglePage
        _                 -> throw404 