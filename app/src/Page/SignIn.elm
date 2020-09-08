port module Page.SignIn exposing (Model, Msg, init, update, subscriptions, view)

import GoogleSignIn
import Element exposing (Element, column, html, text)
import Element.Background as BG
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Json.Encode as E

import Globals as G

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
    Just profile -> column
        [ Element.centerX
        , Element.spacing 10
        ]
        [ text profile.name
        , Input.button
            [ Events.onClick BeginSignOut
            , BG.color G.colorA2
            , Font.color <| G.white
            , Element.focused [ BG.color G.white ]
            , Element.padding 5
            , Element.centerX
            ]
            { label = text "Sign Out"
            , onPress = Nothing
            } 
        ]

    Nothing -> column
        [ Element.centerX
        , Element.spacing 8
        ]
        [ html <| GoogleSignIn.view
            [ GoogleSignIn.onSignIn SignIn
            , GoogleSignIn.idAttr model.client_id
            ]
        ]