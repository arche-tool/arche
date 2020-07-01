module Page.Navegate exposing (Model, Msg(..), main, init, update, subscriptions, view)

import Browser
import Element exposing (Element, column, text, layout, rgb255)
import Element.Border
import Element.Events
import Element.Background as BG
import Html.Attributes
import Http
import Task
import Time
import Json.Decode as D


-- =========== MAIN ===========
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = layout [] << view
    , update = update
    , subscriptions = subscriptions
    }


-- =========== MODEL ===========
type alias Model =
  { token: Maybe String
  , ebsds: List EBSD
  }

type alias EBSD =
  { alias: String
  , hashEBSD: String
  , createdBy: User
  }

type alias User =
  { id_number: String
  , email: Maybe String
  , name: Maybe String
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( {token = Nothing, ebsds = []}
  , Cmd.none
  )


-- =========== UPDATE ===========
type Msg
  = SetToken String
  | ResetToken
  | GetEBSDs 
  | ReceivedEBSDs (Result Http.Error (List EBSD))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetEBSDs ->
        let
          hs = case model.token of
            Just tk -> [Http.header "Authorization" ("Bearer " ++ tk)]
            _       -> []
        in ( model
          , Http.request
            { method = "GET"
            , url = "/api/ebsd"
            , headers = hs
            , body = Http.emptyBody
            , expect = Http.expectJson ReceivedEBSDs ebsdListDecoder
            , timeout = Nothing
            , tracker = Nothing
            }
        )

    ReceivedEBSDs result ->
      case result of
        Err _    -> (model, Cmd.none)
        Ok  ebsds -> ( {model | ebsds = ebsds}, Cmd.none )

    SetToken tk -> (
      {model | token = Just tk},
      Cmd.batch [Task.perform (\_ -> GetEBSDs) Time.now])
    
    ResetToken -> ({model | token = Nothing}, Cmd.none)



-- =========== SUBSCRIPTIONS ===========
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- =========== VIEW ===========
view : Model -> Element Msg
view model =
  case model.token of
    Nothing -> text "Please Sign-in"
    Just _ -> renderEbsds model.ebsds

renderEbsds : List EBSD -> Element Msg
renderEbsds ebsds =
  let
    cols = List.map renderEbsd ebsds
  in column
    [ Element.spacing 10
    , Element.padding 5
    ] cols


insurelloBlue : Element.Color
insurelloBlue = rgb255 59 139 186

renderEbsd : EBSD -> Element Msg
renderEbsd ebsd = column
  [ Element.Events.onClick GetEBSDs
  , Element.Border.rounded 3
  , Element.padding 3
  , Element.pointer
  , BG.color (rgb255 200 100 100)
  , Element.htmlAttribute
    (Html.Attributes.style "user-select" "none")
  , Element.mouseOver
    [ Element.Border.color insurelloBlue
    , Element.Border.glow insurelloBlue 1
    , Element.Border.innerGlow insurelloBlue 1
    ]
  , Element.mouseDown [ Element.alpha 0.6 ]
  ]
  [ text ebsd.alias
  , text ebsd.hashEBSD
  , text (Maybe.withDefault "" ebsd.createdBy.name)
  ]

userDecoder : D.Decoder User
userDecoder =
    D.map3 User
      (D.field "id_number" D.string)
      (D.field "name" (D.nullable D.string))
      (D.field "email" (D.nullable D.string))

ebsdDecoder : D.Decoder EBSD
ebsdDecoder =
    D.map3 EBSD
      (D.field "alias" D.string)
      (D.field "hashEBSD" D.string)
      (D.field "createdBy"userDecoder)

ebsdListDecoder : D.Decoder (List EBSD)
ebsdListDecoder = D.list ebsdDecoder
