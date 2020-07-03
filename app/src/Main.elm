module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html as Html
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
   Just ViewerPage   -> Sub.map Viewer (Viewer.subscriptions model.viewerModel)
   Just UploadPage   -> Sub.map Upload (Upload.subscriptions model.uploadModel)
   Just NavegatePage -> Sub.map Navegate (Navegate.subscriptions model.navegateModel)
   _ -> Sub.none

-- =========== VIEW ===========
view : Model -> Browser.Document Msg
view model =
  let
    page = case model.page of
      Just HomePage     -> text "home"
      Just UploadPage   -> Element.html <| Html.map Upload (Upload.view model.uploadModel)
      Just ViewerPage   -> Element.html <| Html.map Viewer (Viewer.view model.viewerModel)
      Just NavegatePage -> Element.map Navegate (Navegate.view model.navegateModel)
      _                 -> text "???"
    links = 
      [ {label = "home", url = "/", page = HomePage}
      , {label = "Submit new EBSD file", url = "/upload", page = UploadPage}
      , {label = "Reconstructions", url = "/reconstructions", page = NavegatePage}
      , {label = "3D Viewer", url = "/3d-viewer", page = ViewerPage}
      ]
  in
  { title = "Arche"
  , body = [
      layout
        [ Font.color (Element.rgb 0 0 1)
        , Font.size 18
        , Font.family
            [ Font.external
                { name = "MuseoModerno"
                , url = "https://fonts.googleapis.com/css?family=MuseoModerno"
                }
            ]
        , height fill ] <|
        row [ height fill, width fill ]
            [ sidePanel model links
            , page
            ]
    ]
  }

sidePanel : Model -> List PageLink -> Element Msg
sidePanel model channels =
    let
      activeChannelAttrs = [ Background.color <| rgb255 117 179 201, Font.bold ]
      channelAttrs = [ Element.paddingXY 15 5, width fill ]

      channelEl channel = el (
        if channel.page == Maybe.withDefault HomePage model.page
        then activeChannelAttrs ++ channelAttrs
        else channelAttrs
        ) <| link []
          { url = channel.url
          , label = text channel.label
          }
    
      logo = image []
        { src = model.flags.logo
        , description = "logo"
        }
      signIn = Element.map Authentication (SignIn.view model.signinModel)      
    in
    column
        [ height fill
        , width (px 300)
        , paddingXY 0 10
        , scrollbars
        , Background.color <| rgb255 92 99 118
        , Font.color <| rgb255 255 255 255
        ] <| [logo, signIn] ++ List.map channelEl channels

type alias PageLink =
  { url: String
  , label: String
  , page: Page
  } 