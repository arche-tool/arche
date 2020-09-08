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

import Globals as G
import Page.Draw as Viewer
import Page.SignIn as SignIn
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
  , navegateModel : Navegate.Model
  , viewerModel : Viewer.Model
  , signinModel : SignIn.Model
  }


type Page
    = HomePage 
    | ViewerPage
    | NavegatePage


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    (navModel, _) = Navegate.init ()
    (signModel, _) = SignIn.init () flags.oauth_azp
    viewerModel = Viewer.init
    model =
      { key = key
      , url = url
      , page = parseUrl url
      , flags = flags
      , navegateModel = navModel
      , viewerModel = viewerModel
      , signinModel = signModel
      }
  in ( model, Cmd.none )

-- =========== UPDATE ===========
type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
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
            Task.perform (\_ -> Navegate (Navegate.SetToken profile.idToken)) Time.now,
            Cmd.map Authentication newCmd
            ]
          _ -> Cmd.batch [
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
        , Url.map NavegatePage (Url.s "reconstructions")
        , Url.map ViewerPage (Url.s "3d-viewer")
        ]

-- =========== SUBSCRIPTIONS ===========
subscriptions : Model -> Sub Msg
subscriptions model = case model.page of
   Just ViewerPage   -> Sub.map Viewer (Viewer.subscriptions model.viewerModel)
   Just NavegatePage -> Sub.map Navegate (Navegate.subscriptions model.navegateModel)
   _ -> Sub.none

-- =========== VIEW ===========
view : Model -> Browser.Document Msg
view model =
  let
    page = case model.page of
      Just HomePage     -> text "home"
      Just ViewerPage   -> Element.html <| Html.map Viewer (Viewer.view model.viewerModel)
      Just NavegatePage -> Element.map Navegate (Navegate.view model.navegateModel)
      _                 -> text "???"
    links = 
      [ {label = "home", url = "/", page = HomePage}
      , {label = "Reconstructions", url = "/reconstructions", page = NavegatePage}
      , {label = "3D Viewer", url = "/3d-viewer", page = ViewerPage}
      ]
  in
  { title = "Arche"
  , body = [
      layout
        [ Font.color G.white
        , Font.size 16
        , Font.family
            [ Font.external
                { name = "Raleway"
                , url = "https://fonts.googleapis.com/css2?family=Raleway:wght@500"
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
      activeChannelAttrs = [ Background.color <| G.black, Font.bold ]
      channelAttrs = [ Element.paddingXY 15 5, width fill ]

      channelEl channel = el (
        if channel.page == Maybe.withDefault HomePage model.page
        then activeChannelAttrs ++ channelAttrs
        else channelAttrs
        ) <| link []
          { url = channel.url
          , label = text channel.label
          }
    
      logo = image
        [ centerX ]
        { src = model.flags.logo
        , description = "logo"
        }
      signIn = Element.map Authentication (SignIn.view model.signinModel)      
    in
    column
        [ height fill
        , width (px 175)
        , paddingXY 8 10
        , spacing 10
        , Background.color <| G.black
        , Font.color <| G.white
        ] <| [logo, signIn] ++ List.map channelEl channels

type alias PageLink =
  { url: String
  , label: String
  , page: Page
  } 