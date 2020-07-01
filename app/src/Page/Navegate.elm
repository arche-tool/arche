module Page.Navegate exposing (Model, Msg(..), main, init, update, subscriptions, view)

import Browser
import Html exposing (Html, div, input, progress, button, h1, text)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as D
import Json.Encode as JE


-- =========== MAIN ===========
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
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

    SetToken tk -> ({model | token = Just tk}, Cmd.none)
    
    ResetToken -> ({model | token = Nothing}, Cmd.none)



-- =========== SUBSCRIPTIONS ===========
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- =========== VIEW ===========
view : Model -> Html Msg
view model =
  case model.token of
    Nothing -> div [] [text "Please Sign-in"]
    Just _ -> renderEbsds model.ebsds

renderEbsds : List EBSD -> Html Msg
renderEbsds ebsds = div []
    [ button [ E.onClick GetEBSDs ] [ text "Refresh" ]
    , div [] (List.map renderEbsd ebsds)
    ]

renderEbsd : EBSD -> Html Msg
renderEbsd ebsd = div []
  [ Html.text ebsd.alias
  , Html.text ebsd.hashEBSD
  , Html.text (Maybe.withDefault "" ebsd.createdBy.name)
  ]

type alias StorageLink =
  { objectName : String
  , signedLink : String
  }

storageLinkDecoder : D.Decoder StorageLink
storageLinkDecoder =
    D.map2 StorageLink
      (D.field "objectName" D.string)
      (D.field "signedLink" D.string)

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
