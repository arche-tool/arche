module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, text, div, li, ul, img, nav)
import Html.Attributes exposing (href, id, src)
import Platform.Cmd as Cmd
import Task
import Time
import Url
import Url.Parser as Url

import Page.Draw as Viewer
import Page.SignIn as SignIn
import Page.Upload as Upload
import Page.Navegate as Navegate

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
  , oauth_azp : String
  }

type alias Model =
  { key  : Nav.Key
  , url  : Url.Url
  , page : Maybe Page
  , flags : Flags
  , uploadModel : Upload.Model
  , navegateModel : Navegate.Model
  , viewerModel : Viewer.Model
  , signinModel : SignIn.Model
  }


type Page
    = HomePage 
    | UploadPage
    | ViewerPage
    | NavegatePage


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    (upModel, _) = Upload.init ()
    (navModel, _) = Navegate.init ()
    (signModel, _) = SignIn.init () flags.oauth_azp
    viewerModel = Viewer.init
    model =
      { key = key
      , url = url
      , page = parseUrl url
      , flags = flags
      , uploadModel = upModel
      , navegateModel = navModel
      , viewerModel = viewerModel
      , signinModel = signModel
      }
  in ( model, Cmd.none )

-- =========== UPDATE ===========
type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | Upload Upload.Msg
  | Navegate Navegate.Msg
  | Viewer Viewer.Msg
  | Authentication SignIn.Msg

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
    
    Navegate navmsg ->
      let
        (newModel, newCmd) = Navegate.update navmsg model.navegateModel
      in ( {model | navegateModel = newModel }, Cmd.map Navegate newCmd )

    Viewer vimsg ->
      let
        (newModel, newCmd) = Viewer.update vimsg model.viewerModel
      in ( {model | viewerModel = newModel }, Cmd.map Viewer newCmd )

    Authentication signmsg ->
      let
        (newModel, newCmd) = SignIn.update signmsg model.signinModel
        cmds = case newModel.profile of
          Just profile -> Cmd.batch [
            Task.perform (\_ -> Upload (Upload.SetToken profile.idToken)) Time.now,
            Task.perform (\_ -> Navegate (Navegate.SetToken profile.idToken)) Time.now,
            Cmd.map Authentication newCmd
            ]
          _ -> Cmd.batch [
            Task.perform (\_ -> Upload Upload.ResetToken) Time.now,
            Task.perform (\_ -> Navegate Navegate.ResetToken) Time.now,
            Cmd.map Authentication newCmd
            ]
      in ( {model | signinModel = newModel }, cmds )


parseUrl : Url.Url -> Maybe Page
parseUrl = Url.parse route

route : Url.Parser (Page -> a) a
route =
    Url.oneOf
        [ Url.map HomePage Url.top
        , Url.map UploadPage (Url.s "upload")
        , Url.map NavegatePage (Url.s "reconstructions")
        , Url.map ViewerPage (Url.s "3d-viewer")
        ]

-- =========== SUBSCRIPTIONS ===========
subscriptions : Model -> Sub Msg
subscriptions model = case model.page of
   Just ViewerPage -> Sub.map Viewer (Viewer.subscriptions model.viewerModel)
   Just UploadPage -> Sub.map Upload (Upload.subscriptions model.uploadModel)
   Just NavegatePage -> Sub.map Navegate (Navegate.subscriptions model.navegateModel)
   _ -> Sub.none

-- =========== VIEW ===========
view : Model -> Browser.Document Msg
view model =
  let
    fixed = 
      [ sidebar model
      ]
    page = case model.page of
      Just HomePage -> [text "home"
        ]
      Just UploadPage -> [Html.map Upload (Upload.view model.uploadModel)]
      Just ViewerPage -> [Html.map Viewer (Viewer.view model.viewerModel)]
      Just NavegatePage -> [Html.map Navegate (Navegate.view model.navegateModel)]
      _ -> [text "???"]
  in
  { title = "Arche"
  , body = fixed ++ page
  }

sidebar : Model -> Html Msg
sidebar model = nav [id "sidebar"]
  [ div []
    [ img [ src model.flags.logo ] [] ]
    , Html.map Authentication (SignIn.view model.signinModel)
    , ul []
      [ viewLink "home" "/"
      , viewLink "Submit new EBSD file" "/upload"
      , viewLink "Reconstructions" "/reconstructions"
      , viewLink "3D Viewer" "/3d-viewer"
      ]
  ]


viewLink : String -> String -> Html msg
viewLink name path =
  li [] [ a [ href path ] [ text name ] ]