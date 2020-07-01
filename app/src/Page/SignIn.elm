port module Page.SignIn exposing (Model, Msg, init, update, subscriptions, view)

import GoogleSignIn
import Element exposing (Element, column, html, text)
import Html
import Html.Events exposing (onClick)
import Json.Encode as E


port googleSignOut : E.Value -> Cmd msg

port googleSignOutComplete : (E.Value -> msg) -> Sub msg

-- MODEL
type alias Model =
    { profile : Maybe GoogleSignIn.Profile
    , client_id : GoogleSignIn.ClientId
    }

init : () -> String -> ( Model, Cmd msg )
init () azp = ( {client_id = GoogleSignIn.Id azp, profile = Nothing}, Cmd.none )

-- UPDATE
type Msg
    = SignIn GoogleSignIn.Profile
    | BeginSignOut
    | SignOutComplete

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignIn profile ->
            ( { model | profile = Just profile }, Cmd.none )

        BeginSignOut ->
            ( model, googleSignOut <| GoogleSignIn.encodeId model.client_id )

        SignOutComplete ->
            ( { model | profile = Nothing }, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ = googleSignOutComplete (\_ -> SignOutComplete)

-- VIEW
view : Model -> Element Msg
view model = case model.profile of
    Just profile -> column []
        [ text profile.name
        , html <| Html.button [ onClick BeginSignOut ] [ Html.text "Sign Out" ] 
        ]

    Nothing -> column []
        [ text "Sign-in here"
        , html <| GoogleSignIn.view
            [ GoogleSignIn.onSignIn SignIn
            , GoogleSignIn.idAttr model.client_id
            ]
        ]
