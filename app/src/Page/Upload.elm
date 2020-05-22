module Page.Upload exposing (Model, Msg(..), main, init, update, subscriptions, view)

import Browser
import File exposing (File)
import Html exposing (Html, div, input, progress, button, h1, text)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as D


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
  , state: UploadState
  }

type UploadState
  = Waiting
  | Uploading Float
  | Done
  | Fail


init : () -> (Model, Cmd Msg)
init _ =
  ( {token = Nothing, state = Waiting}
  , Cmd.none
  )


-- =========== UPDATE ===========
type Msg
  = SetToken String
  | ResetToken
  | GotFiles (List File)
  | GotProgress Http.Progress
  | Uploaded (Result Http.Error ())
  | Cancel


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotFiles files ->
      let
        hs = case model.token of
          Just tk -> [Http.header "Authorization" ("Bearer " ++ tk)]
          _       -> []
      in ( {model | state = Uploading 0}
         , Http.request
          { method = "POST"
          , url = "/api/upload"
          , headers = hs
          , body = Http.multipartBody (List.map (Http.filePart "files[]") files)
          , expect = Http.expectWhatever Uploaded
          , timeout = Nothing
          , tracker = Just "upload"
          }
      )

    GotProgress progress ->
      case progress of
        Http.Sending p ->
          ({model | state = Uploading (Http.fractionSent p)}, Cmd.none)

        Http.Receiving _ ->
          (model, Cmd.none)

    Uploaded result ->
      case result of
        Ok _ ->
          ({model | state = Done}, Cmd.none)

        Err _ ->
          ({model | state = Fail}, Cmd.none)

    Cancel ->
      ({model | state = Waiting}, Http.cancel "upload")

    SetToken tk -> ({model | token = Just tk}, Cmd.none)
    
    ResetToken -> ({model | token = Nothing}, Cmd.none)



-- =========== SUBSCRIPTIONS ===========
subscriptions : Model -> Sub Msg
subscriptions _ =
  Http.track "upload" GotProgress


-- =========== VIEW ===========
view : Model -> Html Msg
view model =
  case model.token of
    Nothing -> div [] [text "Please Sign-in"]
    Just _ -> renderState model.state

renderState : UploadState -> Html Msg
renderState state =
  case state of
    Waiting ->
      input
        [ A.type_ "file"
        , A.multiple True
        , E.on "change" (D.map GotFiles filesDecoder)
        ]
        []

    Uploading fraction ->
      div []
        [ progress
            [ A.value (String.fromInt (round (100 * fraction)))
            , A.max "100"
            , A.style "display" "block"
            ]
            []
        , button [ E.onClick Cancel ] [ text "Cancel" ]
        ]

    Done ->
      h1 [] [ text "DONE" ]

    Fail ->
      h1 [] [ text "FAIL" ]

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)
