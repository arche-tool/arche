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
  , orEvals: List OREval
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
    , orEvals = []
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
  | RefreshORs String
  | SelectedEBSD String
  | SetORConfig ORConfig
  | SubmitORConfig ORConfig
  | NewOR String (Result Http.Error OREval)
  | ReceivedEBSDs (Result Http.Error (List EBSD))
  | ReceivedORs String (Result Http.Error (List OREval))

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
                 , expect = Http.expectJson (NewOR ebsdHash) orEvalDecoder
                 , timeout = Nothing
                 , tracker = Nothing
                 }
                 )
            _ -> (model, Cmd.none)
        _ -> (model, Cmd.none)

    RefreshORs ebsdHash ->
      case model.token of
        Just tk ->
          let
            hs = [Http.header "Authorization" ("Bearer " ++ tk)]
          in ( model
              , Http.request
              { method = "GET"
              , url = "/api/ebsd/hash/" ++ ebsdHash ++ "/orfit"
              , headers = hs
              , body = Http.emptyBody
              , expect = Http.expectJson (ReceivedORs ebsdHash) (D.list orEvalDecoder)
              , timeout = Nothing
              , tracker = Nothing
              }
              )
        _ -> (model, Cmd.none)
  
    SelectedEBSD hash -> (
      {model | selectedEBSDHash = Just hash},
      Cmd.batch [Task.perform (\_ -> RefreshORs hash) Time.now])

    ReceivedEBSDs result ->
      case result of
        Err _    -> (model, Cmd.none)
        Ok  ebsds -> ( {model | ebsds = ebsds}, Cmd.none )

    SetToken tk -> (
      {model | token = Just tk},
      Cmd.batch [Task.perform (\_ -> RefreshEBSDs) Time.now])
    
    ResetToken -> ({model | token = Nothing}, Cmd.none)
   
    SetORConfig orCfg -> ({model | orCfgInput = orCfg}, Cmd.none)
    
    NewOR _ _ -> (model, Cmd.none)

    ReceivedORs _ result ->
      case result of
        Err _   -> (model, Cmd.none)
        Ok  ors -> ({model | orEvals = ors}, Cmd.none)

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
      , renderORs model.orEvals Nothing
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

renderORs : List OREval -> Maybe String -> Element Msg
renderORs orEval selectedHash =
  let
    isSelected = \e -> Maybe.withDefault False <| Maybe.map (\s -> s == e.hashOR) selectedHash
    cols = List.map (\e -> renderOREval e (isSelected e)) orEval
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

renderOREval : OREval -> Bool -> Element Msg
renderOREval orEval isSelected = column
  [ Element.Border.rounded 3
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
  [ text (degToText orEval.resultOR.misfitError.avgError)
  , text orEval.hashOR
  ]

degToText : Deg -> String
degToText v = format {base | decimals = Exact 1} v.unDeg

renderORInput : ORConfig -> Bool -> Element Msg
renderORInput orCfg isSelected =
  let
    degValue = degToText orCfg.misoAngle
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

type alias Deg =
  { unDeg: Float
  }
 
type alias AxisPair =
  { axis: (Float, Float, Float)
  , angle: Float
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

degEncoder : Deg -> JE.Value
degEncoder deg = JE.object
  [ ("unDeg", JE.float deg.unDeg) ]

axisPairEncoder : AxisPair -> JE.Value
axisPairEncoder ap =
  let
    (x, y, z) = ap.axis
  in JE.object
      [ ("axisAngle", JE.list identity [JE.list JE.float [x, y, z], JE.float ap.angle])
    ] 

orCfgDecoder : D.Decoder ORConfig
orCfgDecoder =
    D.map3 ORConfig
      (D.field "misoAngle" degDecoder)
      (D.field "optByAvg" D.bool)
      (D.field "predefinedOR" <| D.maybe axisPairDecoder)

degDecoder : D.Decoder Deg
degDecoder = D.map Deg (D.field "unDeg" D.float)

axisPairDecoder : D.Decoder AxisPair
axisPairDecoder = D.field "axisAngle" (D.map2 AxisPair tuple3Fdec D.float)

tuple3Fdec : D.Decoder (Float, Float, Float)
tuple3Fdec = D.map3 (\a b c -> (a,b,c)) (D.index 0 D.float) (D.index 1 D.float) (D.index 2 D.float)

tuple3Idec : D.Decoder (Int, Int, Int)
tuple3Idec = D.map3 (\a b c -> (a,b,c)) (D.index 0 D.int) (D.index 1 D.int) (D.index 2 D.int)

type alias OREval =
  { cfgOR: ORConfig
  , resultOR: ResultOR
  , hashOR: String
  }

type alias ResultOR =
  { orientationRelationship: OrientationRelationship
  , ksDeviation: KSDeviation
  , misfitError: FitError
  }
  
type alias OrientationRelationship =
  { orAngle: Deg
  , orAxis: (Int, Int, Int)
  }

type alias KSDeviation =
  { planeDeviation: Deg
  , axisDeviation: Deg
  , directDeviation: Deg
  }

type alias FitError =
  { maxError: Deg
  , devError: Deg
  , avgError: Deg
  }

orEvalDecoder : D.Decoder OREval
orEvalDecoder =
    D.map3 OREval
      (D.field "cfgOR" orCfgDecoder)
      (D.field "resultOR" resultORDecoder)
      (D.field "hashOR" D.string)

resultORDecoder : D.Decoder ResultOR
resultORDecoder =
    D.map3 ResultOR
      (D.field "orientationRelationship" orientationRelationshipDecoder)
      (D.field "ksDeviation" ksDeviationDecoder)
      (D.field "misfitError" fitErrorDecoder)

orientationRelationshipDecoder : D.Decoder OrientationRelationship
orientationRelationshipDecoder =
    D.map2 OrientationRelationship
      (D.field "orAngle" degDecoder)
      (D.field "orAxis" tuple3Idec )

ksDeviationDecoder : D.Decoder KSDeviation
ksDeviationDecoder =
    D.map3 KSDeviation
      (D.field "planeDeviation" degDecoder)
      (D.field "axisDeviation" degDecoder)
      (D.field "directDeviation" degDecoder)

fitErrorDecoder : D.Decoder FitError
fitErrorDecoder =
    D.map3 FitError
      (D.field "maxError" degDecoder)
      (D.field "devError" degDecoder)
      (D.field "avgError" degDecoder)

-- =========== Colors ========
blue = Element.rgb255 238 238 238
purple = Element.rgb255 138 138 238