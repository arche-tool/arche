module Page.Navegate exposing (
  Model,
  Msg(..), boxShape,
  renderEbsds,
  main,
  init,
  update,
  subscriptions,
  view
  )

import Array exposing (Array)
import Browser
import Element exposing (Element, column, text, layout, row, el)
import Element.Background as BG
import Element.Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Http
import Task
import Time

import Globals as G

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
  archeListDecoder)

import Type.Texture exposing (Deg, PhaseSymm(..))

import Type.ArcheTree as ArcheTree
import Type.ArcheTree exposing (ArcheTree)
import Page.Upload as Upload
import Utils exposing (..)

import Widget.ArcheResultExplorer exposing
  ( ArcheResultExplorer
  , newResult
  , updateMcl 
  , updateType 
  , renderResultExplorer
  , ResultType (..)
  )

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
  , archeResultView: Maybe ArcheResultExplorer
  , uploadInput: Maybe Upload.Model
  , orCfgInput: Maybe ORConfig
  , archeCfgInput: Maybe ArcheCfg
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { token = Nothing
    , archeTree = ArcheTree.empty
    , archeResultView = Nothing
    , uploadInput = Nothing
    , orCfgInput = Nothing
    , archeCfgInput = Nothing
    }
  , Cmd.none
  )

defaultORCfg : ORConfig
defaultORCfg =
  { misoAngle    = { unDeg = 5.0 }
  , optByAvg     = False
  , predefinedOR = Nothing
  , startOR      = Nothing
  , parentPhase  = Nothing
  , productPhase = {phaseId = 1, phaseSymm = CubicPhase}
  }

defaultArcheCfg : ArcheCfg
defaultArcheCfg = 
  { misoAngle              = Deg 5.0 
  , excludeFloatingGrains  = True
  , refinementSteps        = 5
  , initClusterFactor      = 1.25
  , stepClusterFactor      = 1.2
  , badAngle               = Deg 15.0
  , parentPhase            = Nothing
  , productPhase           = {phaseId = 1, phaseSymm = CubicPhase}
  }

-- =========== UPDATE ===========
type Msg
  = SetToken String
  | ResetToken

  | RefreshEBSDs 
  | RefreshORs String
  | RefreshArches String String

  | SelectedEBSD String
  | SelectedOR String
  | SelectedArche String

  | SetORConfig (Maybe ORConfig)
  | SetArcheConfig (Maybe ArcheCfg)
  | SetResultMCL Float
  | SetResultType ResultType

  | SubmitEBSDFile (Maybe Upload.Msg)
  | SubmitORConfig ORConfig
  | SubmitArche ArcheCfg

  | ReceivedEBSDs                (Result Http.Error (Array EBSD))
  | ReceivedORs    String        (Result Http.Error (Array OREval))
  | ReceivedArches String String (Result Http.Error (Array Arche))

  | ReceivedNewOR String (Result Http.Error OREval)

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
                 , expect = Http.expectJson (ReceivedNewOR ebsdHash) orEvalDecoder
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
  
    RefreshArches ebsdHash orHash ->
      case model.token of
        Just tk ->
          let
            hs = [Http.header "Authorization" ("Bearer " ++ tk)]
          in ( model
              , Http.request
              { method = "GET"
              , url = "/api/ebsd/hash/" ++ ebsdHash ++ "/orfit/hash/" ++ orHash ++ "/arche"
              , headers = hs
              , body = Http.emptyBody
              , expect = Http.expectJson (ReceivedArches ebsdHash orHash) archeListDecoder
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
      case ArcheTree.getEBSDFocusKey model.archeTree of
         Just ebsdHash -> Cmd.batch [Task.perform (\_ -> RefreshArches ebsdHash hash) Time.now]
         Nothing       -> Cmd.none
      )

    SelectedArche hash ->
      let
        newAT = ArcheTree.focusOnArche model.archeTree hash
      in (
      { model
      | archeTree = newAT
      , archeResultView =  Maybe.map newResult (ArcheTree.getArcheFocus newAT) },
      Cmd.batch [Task.perform (\_ -> RefreshORs hash) Time.now])

    ReceivedEBSDs result ->
      case result of
        Err _     -> (model, Cmd.none)
        Ok  ebsds ->
          let
            at = ArcheTree.refreshArcheTree model.archeTree ebsds
          in ( {model | archeTree = at}, Cmd.none )

    SetToken tk ->
      ( { model | token = Just tk }
      , Cmd.batch
        [ Task.perform (\_ -> RefreshEBSDs) Time.now
        ]
      )
    
    ResetToken -> ({model | token = Nothing}, Cmd.none)
   
    SetORConfig orCfg -> ({model | orCfgInput = orCfg}, Cmd.none)

    SetArcheConfig archeCfg -> ({model | archeCfgInput = archeCfg}, Cmd.none)

    SetResultMCL mcl -> ({model | archeResultView = Maybe.map (updateMcl mcl) model.archeResultView}, Cmd.none)
   
    SetResultType resTy -> ({model | archeResultView = Maybe.map (updateType resTy) model.archeResultView}, Cmd.none)
    
    ReceivedNewOR _ _ -> (model, Cmd.none)

    ReceivedORs ebsdHash result ->
      case result of
        Err _   -> (model, Cmd.none)
        Ok  ors ->
          let
            at = ArcheTree.refreshOR model.archeTree ebsdHash ors
          in ( {model | archeTree = at}, Cmd.none )


    ReceivedArches ebsdHash orHash result ->
      case result of
        Err _      -> (model, Cmd.none)
        Ok  arches ->
          let
            at = ArcheTree.refreshArche model.archeTree ebsdHash orHash arches
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
                   , url = "/api/ebsd/hash/" ++ ebsdHash ++ "/orfit/hash/" ++ orHash ++ "/arche/async"
                   , headers = hs
                   , body = Http.jsonBody <| archeCfgEncoder orCfg
                   , expect = Http.expectJson (ReceivedNewOR ebsdHash) orEvalDecoder
                   , timeout = Nothing
                   , tracker = Nothing
                   }
                   )
              _ -> (model, Cmd.none)
            _ -> (model, Cmd.none)
        _ -> (model, Cmd.none)
    
    SubmitEBSDFile Nothing      -> ({ model | uploadInput = Nothing }, Cmd.none)
    SubmitEBSDFile (Just upmsg) -> case model.uploadInput of
      Just upModel ->
        let
          (newModel, newCmd) = Upload.update upmsg upModel
        in if Upload.isDone newModel
          then
            ( { model | uploadInput = Nothing }
            , Cmd.batch [Task.perform (\_ -> RefreshEBSDs) Time.now]
            )
          else
            ( {model | uploadInput = Just newModel }
            , Cmd.map (SubmitEBSDFile << Just) newCmd
            )
      Nothing -> ({ model | uploadInput = Just <| Upload.initModelWithToken model.token } , Cmd.none)
    

-- =========== SUBSCRIPTIONS ===========
subscriptions : Model -> Sub Msg
subscriptions model = case model.uploadInput of
  Just upModel -> Sub.map (SubmitEBSDFile << Just) (Upload.subscriptions upModel)
  _            -> Sub.none

-- =========== VIEW ===========
view : Model -> Element Msg
view model =
  case model.token of
    Nothing -> text "Please Sign-in"
    Just _ -> renderArcheTree model


renderArcheTree : Model -> Element Msg 
renderArcheTree model =
  let
    base =
      [ renderEbsds  model
      , renderORs    model
      , renderArches model
      ]
    msgBuilder =
      { selectedMcl = SetResultMCL
      , selectedType = SetResultType
      }
    
    rv = maybe [] (List.singleton << renderResultExplorer msgBuilder) model.archeResultView
  in column
    [ Element.spacing 15
    , Element.alignTop
    ]
    (row [ Element.alignTop] base :: rv) 

renderEbsds : Model -> Element Msg
renderEbsds model =
  let
    cols = ArcheTree.listEBSDWithFocus model.archeTree renderEbsd
    input = renderEBSDUpload model
  in column
    [ Element.spacing 10
    , Element.padding 5
    , Element.width (Element.px 300)
    , Element.alignTop
    ] (input :: cols)

renderORs : Model -> Element Msg
renderORs model =
  let
    cols = ArcheTree.listORWithFocus model.archeTree renderOREval 
    input = renderORInput model
  in column
    [ Element.spacing 10
    , Element.padding 5
    , Element.width (Element.px 300)
    , Element.alignTop
    ] (input :: cols)

renderArches :  Model -> Element Msg
renderArches model =
  let
    cols = ArcheTree.listArchesWithFocus model.archeTree renderArche 
    input = renderArcheInput model
  in column
    [ Element.spacing 10
    , Element.padding 5
    , Element.width (Element.px 300)
    , Element.alignTop
    ] (input :: cols)

renderEbsd : EBSD -> Bool -> Element Msg
renderEbsd ebsd isSelected =
  let
    (sx, sy) = ebsd.info.xystep
  in column
    (Element.Events.onClick (SelectedEBSD ebsd.hashEBSD) :: boxShape isSelected)
    [ cardEnrty "name" ebsd.alias
    , cardEnrty "id" <| ebsd.hashEBSD
    , cardEnrty "cols" <| intToText ebsd.info.cols
    , cardEnrty "rows" <| intToText ebsd.info.rows
    , cardEnrty "step size" <| floatToText (max sx sy) 
    , cardEnrty "upload by" <| Maybe.withDefault "" ebsd.createdBy.name
    ]

renderOREval : OREval -> Bool -> Element Msg
renderOREval orEval isSelected = 
  let
    product = orEval.cfgOR.productPhase
    parenPhase = Maybe.map .phaseId orEval.cfgOR.parentPhase
    parenSymm = Maybe.map .phaseSymm orEval.cfgOR.parentPhase
  in column
    (Element.Events.onClick (SelectedOR orEval.hashOR) :: boxShape isSelected)
    [ cardEnrty "avg. angular misfit" <| degToText orEval.resultOR.misfitError.avgError ++ "°"
    , cardEnrty "<100> <111> deviation" <| degToText orEval.resultOR.ksDeviation.planeDeviation ++ "°"
    , cardEnrty "<100> <111> deviation" <| degToText orEval.resultOR.ksDeviation.axisDeviation ++ "°"
    , cardEnrty "parent phase ID" <| intToText product.phaseId
    , cardEnrty "parent symmetry" <| symmToText product.phaseSymm
    , maybe Element.none (cardEnrty "product phase ID" << intToText) parenPhase
    , maybe Element.none (cardEnrty "product symmetry" << symmToText) parenSymm
    ]

cardEnrty : String -> String -> Element Msg
cardEnrty field value = row
  [ Element.spaceEvenly
  , Element.width Element.fill
  ]
  [ text field, text value ]

renderArche : Arche -> Bool -> Element Msg
renderArche arche isSelected =
  let
    attrs =
      Element.Events.onClick (SelectedArche arche.hashArche) :: boxShape isSelected
    elms = 
      [ cardEnrty "steps" <| String.fromInt arche.cfgArche.refinementSteps
      , cardEnrty "exclude floating grains" <| if arche.cfgArche.excludeFloatingGrains then "☑" else "☐"
      , cardEnrty "initial cluster factor" <| floatToText arche.cfgArche.initClusterFactor
      , cardEnrty "incremental cluster factor" <| floatToText arche.cfgArche.stepClusterFactor
      , cardEnrty "angular misfit threshold" <| degToText arche.cfgArche.badAngle
      ]
  in column attrs elms


renderORInput : Model -> Element Msg
renderORInput model =
  let
    isAvgCheckbox orCfg = Input.checkbox []
      { onChange = \isAvg -> SetORConfig <| Just { orCfg | optByAvg = isAvg}
      , icon = Input.defaultCheckbox
      , checked = orCfg.optByAvg
      , label = Input.labelRight [] (text "Use avg orientation?")
      }

    misoSlider orCfg = Input.slider
      [ Element.height (Element.px 20)
      -- Here is where we're creating/styling the "track"
      , Element.behindContent
          (Element.el
              [ Element.width Element.fill
              , Element.height (Element.px 10)
              , Element.centerY
              , BG.color G.white
              ]
              Element.none
          )
      ]
      { onChange = \deg -> SetORConfig <| Just { orCfg | misoAngle = {unDeg = deg}}
      , label = Input.labelAbove [] (text <| "Misorientation Angle = " ++ degToText orCfg.misoAngle ++ "°")
      , min = 0.1
      , max = 15
      , step = Just 0.1
      , value = orCfg.misoAngle.unDeg
      , thumb = Input.defaultThumb
      }
    phaseSel name orCfg getter setter = Input.radio
      []
      { onChange = \phid -> SetORConfig <| Just (setter orCfg phid)
      , selected = getter orCfg
      , label = Input.labelAbove [] (text <| "Phase ID (" ++ name ++ ")")
      , options = maybe [] (.info >> .phases >> Array.toList >> List.map (\x -> Input.option x.numID (text x.name)))
               <| ArcheTree.getEBSDFocus model.archeTree
      }
 
    symmSel name orCfg getter setter = Input.radio
      []
      { onChange = \symm -> SetORConfig <| Just (setter orCfg symm)
      , selected = getter orCfg
      , label = Input.labelAbove [] (text <| "Symmetry (" ++ name ++ ")")
      , options =
          [ Input.option HexagonalPhase (text "hcp")
          , Input.option CubicPhase     (text "bcc/fcc")
          ]
      }
  in case model.orCfgInput of
    Just orCfg -> column
      boxInputShape
      [ phaseSel "parent"  orCfg (.parentPhase >> Maybe.map .phaseId)   (\x phid -> {x | parentPhase = Just <| {phaseId = phid, phaseSymm = maybe CubicPhase .phaseSymm x.parentPhase}})
      , symmSel  "parent"  orCfg (.parentPhase >> Maybe.map .phaseSymm) (\x symm -> {x | parentPhase = Just <| {phaseId = maybe 1 .phaseId x.parentPhase, phaseSymm = symm}})
      , phaseSel "product" orCfg (.productPhase >> .phaseId >> Just)    (\x phid -> {x | productPhase = {phaseId = phid, phaseSymm = .phaseSymm x.productPhase}})
      , symmSel  "product" orCfg (.productPhase >> .phaseSymm >> Just)  (\x symm -> {x | productPhase = {phaseId = .phaseId x.productPhase, phaseSymm = symm}})
      , isAvgCheckbox orCfg
      , misoSlider orCfg
      , submitButton (SubmitORConfig orCfg)
      , toogleInput SetORConfig Nothing
      ]
    Nothing -> if ArcheTree.hasEBSDFocus model.archeTree
      then toogleInput SetORConfig
        <| Just
        <| maybe defaultORCfg .cfgOR
        <| ArcheTree.getORFocus model.archeTree
      else Element.none

renderArcheInput : Model -> Element Msg
renderArcheInput model =
  let
    excludeFloatingCheckbox archeCfg = Input.checkbox []
      { onChange = \x -> SetArcheConfig <| Just { archeCfg | excludeFloatingGrains = x } 
      , icon = Input.defaultCheckbox
      , checked = archeCfg.excludeFloatingGrains
      , label = Input.labelRight [] (text "Exclude Floating Grains?")
      }

    stepsSlider archeCfg = Input.slider
      [ Element.height (Element.px 20)
      -- Here is where we're creating/styling the "track"
      , Element.behindContent
          (Element.el
              [ Element.width Element.fill
              , Element.height (Element.px 10)
              , Element.centerY
              , BG.color G.white
              ]
              Element.none
          )
      ]
      { onChange = \x -> SetArcheConfig <| Just { archeCfg | refinementSteps = Basics.round x } 
      , label = Input.labelAbove [] (text <| "Number of steps: " ++ intToText archeCfg.refinementSteps)
      , min = 1
      , max = 10
      , step = Just 1.0
      , value = toFloat archeCfg.refinementSteps
      , thumb = Input.defaultThumb
      }
  in case model.archeCfgInput of
    Just archeCfg -> column
      boxInputShape
      [ excludeFloatingCheckbox archeCfg
      , stepsSlider archeCfg
      , submitButton (SubmitArche archeCfg)
      , toogleInput SetArcheConfig Nothing
      ]
    Nothing -> if ArcheTree.hasORFocus model.archeTree
      then toogleInput SetArcheConfig
        <| Just
        <| maybe defaultArcheCfg .cfgArche
        <| ArcheTree.getArcheFocus model.archeTree
      else Element.none

renderEBSDUpload : Model -> Element Msg
renderEBSDUpload model =
  case model.uploadInput of
    Just upModel -> column
      boxInputShape
      [ Element.map (SubmitEBSDFile << Just) <| Element.html (Upload.view upModel)
      , if Upload.isUploading upModel
        then Element.none
        else toogleInput SubmitEBSDFile Nothing
      ]
    Nothing -> toogleInput SubmitEBSDFile (Just Upload.Cancel)

-- =========== commum ============

boxShape : Bool -> List (Element.Attribute msg)
boxShape isSelected =
  [ Element.Border.rounded 3
  , Element.padding 10
  , Element.spacing 3
  , Element.width Element.fill
  , Element.pointer
  , BG.color (if isSelected then G.colorA1 else G.colorA)
  , Font.color <| if isSelected then G.white else G.black
  , Element.htmlAttribute
    (Html.Attributes.style "user-select" "none")
  , Element.mouseOver
    [ Element.Border.color G.black
    , Element.Border.glow G.black 1
    , Element.Border.innerGlow G.black 1
    ]
  , Element.mouseDown [ Element.alpha 0.6 ]
  ]

boxInputShape : List (Element.Attribute msg)
boxInputShape =
  [ Element.Border.rounded 3
  , Element.padding 10
  , Element.width Element.fill
  , Element.spacing 10
  , Element.pointer
  , BG.color G.colorA1
  , Element.htmlAttribute (Html.Attributes.style "user-select" "none")
  ]

-- =========== Widgets ===========
toogleInput : (Maybe a -> msg) -> Maybe a -> Element msg
toogleInput func value =
  let
    color = case value of
      Just _ -> G.colorB
      _      -> G.colorB 
  in Input.button
    [ Font.size 20
    , Element.centerX
    , Font.color <| color
    ]
    { onPress = Just (func value)
    , label = text <| case value of
      Just _ -> "✚"
      _      -> "✖" 
    }

submitButton : msg -> Element msg
submitButton x = Input.button
  [ BG.color G.colorA2
  , Element.padding 5
  ]
  { onPress = Just x
  , label = text "Submit"
  }
