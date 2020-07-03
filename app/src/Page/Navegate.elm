module Page.Navegate exposing (Model, Msg(..), main, init, update, subscriptions, view)

import Browser
import Element exposing (Element, column, text, layout, rgb255, row)
import Element.Background as BG
import Element.Border
import Element.Events
import Element.Input as Input
import Html.Attributes
import Http
import Task
import Time
import Json.Decode as D
import Json.Encode as JE
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), base)

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
  , selectedEBSDHash: Maybe String
  , orCfgInput: ORConfig
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
  ( { token = Nothing
    , ebsds = []
    , selectedEBSDHash = Nothing
    , orCfgInput = defaultORCfg
    }
  , Cmd.none
  )

defaultORCfg : ORConfig
defaultORCfg =
  { misoAngle = { unDeg = 5.0 }
  , optByAvg = False
  , predefinedOR = Nothing
  }

-- =========== UPDATE ===========
type Msg
  = SetToken String
  | ResetToken
  | RefreshEBSDs 
  | SelectedEBSD String
  | SetORConfig ORConfig
  | SubmitORConfig ORConfig
  | ReceivedEBSDs (Result Http.Error (List EBSD))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RefreshEBSDs ->
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

    SubmitORConfig orCfg ->
      case model.token of
        Just tk ->
          case model.selectedEBSDHash of
            Just ebsdHash ->
              let
                hs = [Http.header "Authorization" ("Bearer " ++ tk)]
              in ( model
                 , Http.request
                 { method = "POST"
                 , url = "/api/ebsd/hash/" ++ ebsdHash ++ "/orfit"
                 , headers = hs
                 , body = Http.jsonBody <| orCfgEncoder orCfg
                 , expect = Http.expectJson ReceivedEBSDs ebsdListDecoder
                 , timeout = Nothing
                 , tracker = Nothing
                 }
                 )
            _ -> (model, Cmd.none)
        _ -> (model, Cmd.none)

    SelectedEBSD hash -> ({model | selectedEBSDHash = Just hash}, Cmd.none)

    ReceivedEBSDs result ->
      case result of
        Err _    -> (model, Cmd.none)
        Ok  ebsds -> ( {model | ebsds = ebsds}, Cmd.none )

    SetToken tk -> (
      {model | token = Just tk},
      Cmd.batch [Task.perform (\_ -> RefreshEBSDs) Time.now])
    
    ResetToken -> ({model | token = Nothing}, Cmd.none)
   
    SetORConfig orCfg -> ({model | orCfgInput = orCfg}, Cmd.none)


-- =========== SUBSCRIPTIONS ===========
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- =========== VIEW ===========
view : Model -> Element Msg
view model =
  case model.token of
    Nothing -> text "Please Sign-in"
    Just _ -> row []
      [ renderEbsds model.ebsds model.selectedEBSDHash
      , renderORInput model.orCfgInput False
      ]

renderEbsds : List EBSD -> Maybe String -> Element Msg
renderEbsds ebsds selectedHash =
  let
    isSelected = \ e -> Maybe.withDefault False <| Maybe.map (\s -> s == e.hashEBSD) selectedHash
    cols = List.map (\e -> renderEbsd e (isSelected e)) ebsds
  in column
    [ Element.spacing 10
    , Element.padding 5
    ] cols


insurelloBlue : Element.Color
insurelloBlue = rgb255 59 139 186

renderEbsd : EBSD -> Bool -> Element Msg
renderEbsd ebsd isSelected = column
  [ Element.Events.onClick (SelectedEBSD ebsd.hashEBSD)
  , Element.Border.rounded 3
  , Element.padding 3
  , Element.pointer
  , BG.color ( if isSelected then rgb255 100 200 100 else rgb255 200 100 100)
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


renderORInput : ORConfig -> Bool -> Element Msg
renderORInput orCfg isSelected =
  let
    degValue = format {base | decimals = Exact 1} orCfg.misoAngle.unDeg
    isAvgCheckbox = Input.checkbox []
      { onChange = \isAvg -> SetORConfig { orCfg | optByAvg = isAvg}
      , icon = Input.defaultCheckbox
      , checked = orCfg.optByAvg
      , label = Input.labelRight [] (text "Use avg orientation?")
      }

    misoSlider = Input.slider
      [ Element.height (Element.px 20)
      -- Here is where we're creating/styling the "track"
      , Element.behindContent
          (Element.el
              [ Element.width Element.fill
              , Element.height (Element.px 10)
              , Element.centerY
              , BG.color (rgb255 120 120 120)
              ]
              Element.none
          )
      ]
      { onChange = \deg -> SetORConfig { orCfg | misoAngle = {unDeg = deg}}
      , label = Input.labelAbove [] (text <| "Misorientation Angle = " ++ degValue ++ "°")
      , min = 0.1
      , max = 15
      , step = Just 0.1
      , value = orCfg.misoAngle.unDeg
      , thumb = Input.defaultThumb
      }
    submitNewOR = Input.button
      [ BG.color blue
      , Element.focused
          [ BG.color purple ]
      ]
      { onPress = Just (SubmitORConfig orCfg)
      , label = text "+"
      }
  in column
  [ Element.Border.rounded 3
  , Element.padding 10
  , Element.spacing 10
  , Element.pointer
  , BG.color ( if isSelected then rgb255 100 200 100 else rgb255 200 100 100)
  , Element.htmlAttribute
    (Html.Attributes.style "user-select" "none")
  ]
  [ isAvgCheckbox
  , misoSlider
  , submitNewOR
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


type alias ORConfig =
  { misoAngle: Deg
  , optByAvg: Bool
  , predefinedOR: Maybe AxisPair
  }
orCfgEncoder : ORConfig -> JE.Value
orCfgEncoder cfg =
  let
    ls =
      [ ("misoAngle", degEncoder cfg.misoAngle)
      , ("optByAvg", JE.bool cfg.optByAvg)
      ] ++ Maybe.withDefault [] maybeAp 
    maybeAp = Maybe.map (\ap -> [("predefinedOR", axisPairEncoder ap)]) cfg.predefinedOR
  in JE.object ls

type alias Deg =
  { unDeg: Float
  }
degEncoder : Deg -> JE.Value
degEncoder deg = JE.object
  [ ("unDeg", JE.float deg.unDeg) ]
 
type alias AxisPair =
  { axis: (Float, Float, Float)
  , angle: Float
  }

axisPairEncoder : AxisPair -> JE.Value
axisPairEncoder ap =
  let
    (x, y, z) = ap.axis
  in JE.object
      [ ("axisAngle", JE.list identity [JE.list JE.float [x, y, z], JE.float ap.angle])
    ] 

-- =========== Colors ========
blue = Element.rgb255 238 238 238
purple = Element.rgb255 138 138 238