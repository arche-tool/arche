module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Platform.Cmd as Cmd
import Url
import Url.Parser as Url

import Page.Upload as Upload
import Page.Draw as Viewer

-- =========== MAIN ===========
main : Program Flags Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- =========== MODEL ===========
type alias Flags =
  { logo : String
  }

type alias Model =
  { key  : Nav.Key
  , url  : Url.Url
  , page : Maybe Page
  , flags : Flags
  , uploadModel : Upload.Model
  , viewerModel : Viewer.Model
  }


type Page
    = HomePage 
    | UploadPage
    | ViewerPage


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    (upModel, _) = Upload.init ()
    model = Model key url (parseUrl url) flags upModel Viewer.init
  in ( model, Cmd.none )

-- =========== UPDATE ===========
type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | Upload Upload.Msg
  | Viewer Viewer.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url, page = parseUrl url }
      , Cmd.none
      )

    Upload upmsg ->
      let
        (newModel, newCmd) = Upload.update upmsg model.uploadModel
      in ( {model | uploadModel = newModel }, Cmd.map Upload newCmd )
    Viewer vimsg ->
      let
        (newModel, newCmd) = Viewer.update vimsg model.viewerModel
      in ( {model | viewerModel = newModel }, Cmd.map Viewer newCmd )


parseUrl : Url.Url -> Maybe Page
parseUrl = Url.parse route

route : Url.Parser (Page -> a) a
route =
    Url.oneOf
        [ Url.map HomePage Url.top
        , Url.map UploadPage (Url.s "upload")
        , Url.map ViewerPage (Url.s "reconstructions")
        ]

-- =========== SUBSCRIPTIONS ===========
subscriptions : Model -> Sub Msg
subscriptions model = case model.page of
   Just ViewerPage -> Sub.map Viewer (Viewer.subscriptions model.viewerModel)
   Just UploadPage -> Sub.map Upload (Upload.subscriptions model.uploadModel)
   _ -> Sub.none

-- =========== VIEW ===========
view : Model -> Browser.Document Msg
view model =
  let
    fixed = 
      [ sidebar model
      ]
    page = case model.page of
      Just HomePage -> [text "home"]
      Just UploadPage -> [Html.map Upload (Upload.view model.uploadModel)]
      Just ViewerPage -> [Html.map Viewer (Viewer.view model.viewerModel)]
      _ -> [text "???"]
  in
  { title = "Arche"
  , body = fixed ++ page
  }

sidebar : Model -> Html a
sidebar model = nav [id "sidebar"]
  [ div []
    [ img [ src model.flags.logo ] [] ]

  , ul []
    [ viewLink "home" "/"
    , viewLink "Submit new EBSD file" "/upload"
    , viewLink "My reconstructions" "/reconstructions"
    ]
  ]


viewLink : String -> String -> Html msg
viewLink name path =
  li [] [ a [ href path ] [ text name ] ]