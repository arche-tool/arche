module Page.Upload exposing (Model, Msg, main, init, update, subscriptions, view)

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
type Model
  = Waiting
  | Uploading Float
  | Done
  | Fail


init : () -> (Model, Cmd Msg)
init _ =
  ( Waiting
  , Cmd.none
  )


-- =========== UPDATE ===========
type Msg
  = GotFiles (List File)
  | GotProgress Http.Progress
  | Uploaded (Result Http.Error ())
  | Cancel


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotFiles files ->
      ( Uploading 0
      , Http.request
          { method = "POST"
          , url = "/api/upload"
          , headers = []
          , body = Http.multipartBody (List.map (Http.filePart "files[]") files)
          , expect = Http.expectWhatever Uploaded
          , timeout = Nothing
          , tracker = Just "upload"
          }
      )

    GotProgress progress ->
      case progress of
        Http.Sending p ->
          (Uploading (Http.fractionSent p), Cmd.none)

        Http.Receiving _ ->
          (model, Cmd.none)

    Uploaded result ->
      case result of
        Ok _ ->
          (Done, Cmd.none)

        Err _ ->
          (Fail, Cmd.none)

    Cancel ->
      (Waiting, Http.cancel "upload")



-- =========== SUBSCRIPTIONS ===========
subscriptions : Model -> Sub Msg
subscriptions _ =
  Http.track "upload" GotProgress


-- =========== VIEW ===========
view : Model -> Html Msg
view model =
  case model of
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
