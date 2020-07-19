module Page.Navegate exposing (Model, Msg(..), renderEbsds, main, init, update, subscriptions, view)

import Array exposing (Array)
import Browser
import Element exposing (Element, Color, column, text, layout, rgb255, row)
import Element.Background as BG
import Element.Border
import Element.Events
import Element.Input as Input
import Html.Attributes
import Http
import Task
import Time
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), base)

import Type.EBSD exposing (
  EBSD,
  ebsdListDecoder
  )

import Type.OR exposing (
  OREval,
  ORConfig,
  orCfgEncoder,
  orEvalDecoder,
  orEvalListDecoder
  )

import Type.Arche exposing (
  Arche,
  ArcheCfg, 
  archeCfgEncoder,
  archeCfgDecoder)

import Type.Texture exposing (Deg)

import Type.ArcheTree as ArcheTree
import Type.ArcheTree exposing (ArcheTree)

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
  , archeTree: ArcheTree
  , orCfgInput: ORConfig
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { token = Nothing
    , archeTree = ArcheTree.empty
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
  | SelectedOR String
  | SetORConfig ORConfig
  | SubmitORConfig ORConfig
  | SubmitArche ArcheCfg
  | NewOR String (Result Http.Error OREval)
  | ReceivedEBSDs (Result Http.Error (Array EBSD))
  | ReceivedORs String (Result Http.Error (Array OREval))

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
          case ArcheTree.getEBSDFocusKey model.archeTree of
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
              , expect = Http.expectJson (ReceivedORs ebsdHash) orEvalListDecoder
              , timeout = Nothing
              , tracker = Nothing
              }
              )
        _ -> (model, Cmd.none)
  
    SelectedEBSD hash -> (
      {model | archeTree = ArcheTree.focusOnEbsd model.archeTree hash},
      Cmd.batch [Task.perform (\_ -> RefreshORs hash) Time.now])

    SelectedOR hash -> (
      {model | archeTree = ArcheTree.focusOnOR model.archeTree hash},
      Cmd.batch [Task.perform (\_ -> RefreshORs hash) Time.now])

    ReceivedEBSDs result ->
      case result of
        Err _     -> (model, Cmd.none)
        Ok  ebsds ->
          let
            at = ArcheTree.refreshArcheTree model.archeTree ebsds
          in ( {model | archeTree = at}, Cmd.none )

    SetToken tk -> (
      {model | token = Just tk},
      Cmd.batch [Task.perform (\_ -> RefreshEBSDs) Time.now])
    
    ResetToken -> ({model | token = Nothing}, Cmd.none)
   
    SetORConfig orCfg -> ({model | orCfgInput = orCfg}, Cmd.none)
    
    NewOR _ _ -> (model, Cmd.none)

    ReceivedORs ebsdHash result ->
      case result of
        Err _   -> (model, Cmd.none)
        Ok  ors ->
          let
            at = ArcheTree.refreshOR model.archeTree ebsdHash ors
          in ( {model | archeTree = at}, Cmd.none )


    SubmitArche orCfg ->
      case model.token of
        Just tk ->
          case ArcheTree.getEBSDFocusKey model.archeTree of
            Just ebsdHash -> case ArcheTree.getORFocusKey model.archeTree of
              Just orHash ->
                let
                  hs = [Http.header "Authorization" ("Bearer " ++ tk)]
                in ( model
                   , Http.request
                   { method = "POST"
                   , url = "/api/ebsd/hash/" ++ ebsdHash ++ "/orfit/hash/" ++ orHash ++ "/arche"
                   , headers = hs
                   , body = Http.jsonBody <| archeCfgEncoder orCfg
                   , expect = Http.expectJson (NewOR ebsdHash) orEvalDecoder
                   , timeout = Nothing
                   , tracker = Nothing
                   }
                   )
              _ -> (model, Cmd.none)
            _ -> (model, Cmd.none)
        _ -> (model, Cmd.none)

-- =========== SUBSCRIPTIONS ===========
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- =========== VIEW ===========
view : Model -> Element Msg
view model =
  case model.token of
    Nothing -> text "Please Sign-in"
    Just _ -> renderArcheTree model


renderArcheTree : Model -> Element Msg 
renderArcheTree model = row []
  [ renderEbsds model.archeTree
  , renderORs model.archeTree
  , renderORInput model.orCfgInput False
  ]


renderEbsds : ArcheTree -> Element Msg
renderEbsds at =
  let
    cols = ArcheTree.listEBSDWithFocus at renderEbsd
  in column
    [ Element.spacing 10
    , Element.padding 5
    ] cols

renderORs : ArcheTree -> Element Msg
renderORs at =
  let
    cols = ArcheTree.listORWithFocus at renderOREval 
  in column
    [ Element.spacing 10
    , Element.padding 5
    ] cols


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
  [ Element.Events.onClick (SelectedOR orEval.hashOR)
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
  [ text (degToText orEval.resultOR.misfitError.avgError)
  , text orEval.hashOR
  , Input.button
    [ BG.color blue
    , Element.focused
        [ BG.color purple ]
    ]
    { onPress = Just (SubmitArche testArche)
    , label = text "+"
    }
  ]

testArche = 
  { misoAngle              = Deg 5.0 
  , excludeFloatingGrains  = True
  , refinementSteps        = 5
  , initClusterFactor      = 1.25
  , stepClusterFactor      = 1.2
  , badAngle               = Deg 15.0
  , parentPhaseID          = Nothing
  }


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


-- =========== Colors ========
blue : Color
blue = Element.rgb255 238 238 238

purple : Color
purple = Element.rgb255 138 138 238

insurelloBlue : Element.Color
insurelloBlue = rgb255 59 139 186
