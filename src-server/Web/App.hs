{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.App where

import Servant
import Data.Text                     (Text)
import Servant.HTML.Blaze            (HTML)
import Text.Blaze.Html5              (Html)
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

index :: AttributeValue -> Html
index staticPath = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title "Arche"
        H.link ! A.rel "manifest" ! A.href (staticPath <> "/manifest.json")
        H.link ! A.rel "shortcut icon" ! A.href (staticPath <> "/favicon.ico")
        H.link ! A.rel "stylesheet" ! A.href (staticPath <> "/static/css/main.b2840f52.chunk.css")
    H.body $ do
        H.noscript "You need to enable JavaScript to run this app."
        H.div mempty ! A.id "root"
        H.script mempty ! A.src (staticPath <> "/static/js/runtime~main.1b922744.js")
        H.script mempty ! A.src (staticPath <> "/static/js/vendors~main.7aa09ec8.chunk.js")
        H.script mempty ! A.src (staticPath <> "/static/js/main.923e2e6e.chunk.js")

type Url  = Text

type App = (Capture "url" Url :> Get '[HTML] Html) :<|> "static" :> Raw

appServer :: FilePath -> Server App
appServer static = homepage :<|> serveDirectoryWebApp static

homepage :: Url -> Handler Html
homepage _ = pure (index "static")
