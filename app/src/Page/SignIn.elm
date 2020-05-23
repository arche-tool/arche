port module Page.SignIn exposing (Model, Msg, init, update, subscriptions, view)

import GoogleSignIn
import Html exposing (Html, button, div, text)
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
view : Model -> Html Msg
view model =
    div []
        [ case model.profile of
            Just profile ->
                div []
                    [ div [] [ text ("Welcome, " ++ profile.name) ]
                    , div [] [ button [ onClick BeginSignOut ] [ text "Sign Out" ] ]
                    ]

            Nothing ->
                div []
                    [ text "Sign-in here"
                    , GoogleSignIn.view
                        [ GoogleSignIn.onSignIn SignIn
                        , GoogleSignIn.idAttr model.client_id
                        ]
                    ]
        ]