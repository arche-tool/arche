port module Page.SignIn exposing (Model, Msg, init, update, subscriptions, view)

import GoogleSignIn
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Encode as E


id : GoogleSignIn.ClientId
id = GoogleSignIn.Id "772788016429-qil5o5moma9hrajor1bug6r6eh81nnql"

port googleSignOut : E.Value -> Cmd msg

port googleSignOutComplete : (E.Value -> msg) -> Sub msg

-- MODEL
type alias Model =
    Maybe GoogleSignIn.Profile

init : () -> ( Model, Cmd msg )
init () = ( Nothing, Cmd.none )

-- UPDATE
type Msg
    = SignIn GoogleSignIn.Profile
    | BeginSignOut
    | SignOutComplete

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignIn profile ->
            ( Just profile, Cmd.none )

        BeginSignOut ->
            ( model, googleSignOut <| GoogleSignIn.encodeId id )

        SignOutComplete ->
            ( Nothing, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ = googleSignOutComplete (\_ -> SignOutComplete)

-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ case model of
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
                        , GoogleSignIn.idAttr id
                        ]
                    ]
        ]