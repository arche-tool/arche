module Page.Upload exposing (
  Model,
  Msg(..),
  init,
  initModelWithToken,
  isUploading,
  isDone,
  hasFailed,
  update,
  subscriptions,
  renderInput,
  renderProgress
  )

import Browser
import File exposing (File, name)
import File.Select
import Html as Html
import Html.Attributes as A
import Element exposing (Element, column, row)
import Http
import Json.Decode as D
import Json.Encode as JE
import Type.EBSD exposing (EBSD, ebsdDecoder)
import Url exposing (percentEncode)

import Globals

-- =========== MODEL ===========
type alias Model =
  { token: Maybe String
  , state: UploadState
  }

type UploadState
  = Waiting
  | Uploading Float
  | Processing
  | Done EBSD
  | Fail


isUploading : Model -> Bool
isUploading model = case model.state of
  Uploading _ -> True 
  _           -> False

isDone : Model -> Bool
isDone model = case model.state of
  Done _ -> True 
  _      -> False

hasFailed : Model -> Bool
hasFailed model = case model.state of
  Fail -> True 
  _    -> False

init : () -> (Model, Cmd Msg)
init _ =
  ( {token = Nothing, state = Waiting}
  , Cmd.none
  )

initModelWithToken : Maybe String -> Model
initModelWithToken tk = {token = tk, state = Waiting}

-- =========== UPDATE ===========
type Msg
  = SetToken String
  | ResetToken
  | OpenFileSelector
  | GotFile File
  | UploadLink File (Result Http.Error StorageLink)
  | GotProgress Http.Progress
  | Uploaded String StorageLink (Result Http.Error ())
  | Inserted (Result Http.Error EBSD)
  | Cancel


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    OpenFileSelector -> (model, File.Select.file [] GotFile)

    GotFile file ->
      let
        hs = case model.token of
          Just tk -> [Http.header "Authorization" ("Bearer " ++ tk)]
          _       -> []
      in ( {model | state = Uploading 0}
        , Http.request
          { method = "GET"
          , url = "/api/ebsd/upload-link"
          , headers = hs
          , body = Http.emptyBody
          , expect = Http.expectJson (UploadLink file) storageLinkDecoder
          , timeout = Nothing
          , tracker = Just "upload-link"
          }
      )

    UploadLink file result ->
      case result of
        Err _    -> ({model | state = Fail}, Cmd.none)
        Ok  link -> 
          let
            hs = case model.token of
              Just tk -> [Http.header "Authorization" ("Bearer " ++ tk)]
              _       -> []
          in ( {model | state = Uploading 0}
            , Http.request
              { method = "PUT"
              , url = link.signedLink
              , headers = hs
              , body = Http.fileBody file
              , expect = Http.expectWhatever (Uploaded (name file) link)
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

    Uploaded name link result ->
      case result of
        Err _    -> ({model | state = Fail}, Cmd.none)
        Ok  _ -> 
          let
            hs = case model.token of
              Just tk -> [Http.header "Authorization" ("Bearer " ++ tk)]
              _       -> []
          in ( {model | state = Processing}
            , Http.request
              { method = "POST"
              , url = "/api/ebsd?alias=" ++ percentEncode name
              , headers = hs
              , body = Http.jsonBody (JE.object [("objFullName", JE.string link.objectName)])
              , expect = Http.expectJson Inserted ebsdDecoder
              , timeout = Nothing
              , tracker = Just "upload-link"
              }
          )
    
    Inserted result ->
      case result of
        Ok ebsd ->
          ({model | state = Done ebsd}, Cmd.none)

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
view : Model -> Element Msg
view model =
  case model.token of
    Nothing -> Element.none
    Just _  -> renderInput model.state

renderInput : UploadState -> Element Msg
renderInput state =
  case state of
    Waiting -> column Globals.boxInputShape
        [ Element.text "Please, upload the ANG file to process."
        , Globals.renderButton [Element.centerX] "Select files" OpenFileSelector
        ] 
    _ -> Element.none

renderProgress : UploadState -> Element Msg
renderProgress state =
  let
    content = case state of
      Uploading fraction ->
        [ Element.html <| Html.progress
            [ A.value (String.fromInt (round (100 * fraction)))
            , A.max "100"
            , A.style "display" "block"
            ]
            []
        , Globals.renderButton [Element.centerX] "Cancel" Cancel 
        ] 

      Done _ ->
        [ Element.text "DONE"
        ]

      Fail ->
        [ Element.text "FAIL"
        ]

      Processing ->
        [ Element.text "Processing EBSD"
        , Element.html <| Html.div
            [A.class "spinner"] 
            [ Html.div [A.class "bounce1"] []
            , Html.div [A.class "bounce2"] []
            , Html.div [A.class "bounce3"] []
            ]
        ]
      _ -> []
    in column
      Globals.boxInputShape
      content

type alias StorageLink =
  { objectName : String
  , signedLink : String
  }

storageLinkDecoder : D.Decoder StorageLink
storageLinkDecoder =
    D.map2 StorageLink
      (D.field "objectName" D.string)
      (D.field "signedLink" D.string)