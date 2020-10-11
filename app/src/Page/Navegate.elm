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
import Dict exposing (Dict)
import Dict as Dict
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

import Json.Decode as D
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

import Type.Texture exposing (Deg, PhaseSymm(..), Either(..))

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

import API
import Process

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

type InputCfg
  = UCfg Upload.Model
  | OCfg ORConfig
  | ACfg ArcheCfg
  | NoCfg

getUploadModel : InputCfg -> Maybe Upload.Model
getUploadModel inp = case inp of
  UCfg model -> Just model
  _          -> Nothing

getORCfg : InputCfg -> Maybe ORConfig
getORCfg inp = case inp of
  OCfg model -> Just model
  _          -> Nothing

getArcheCfg : InputCfg -> Maybe ArcheCfg
getArcheCfg inp = case inp of
  ACfg model -> Just model
  _          -> Nothing

hasActiveInput : InputCfg -> Bool
hasActiveInput x = case x of
   NoCfg -> False
   _     -> True

type alias Model =
  { token: Maybe String
  , archeTree: ArcheTree
  , archeResultView: Maybe ArcheResultExplorer
  , inputCfg: InputCfg 
  , runningORProcesses: Dict String { ebsdHash : String, count : Int }
  , runningArcheProcesses: Dict String { ebsdHash : String, orHash : String, count : Int }
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { token = Nothing
    , archeTree = ArcheTree.empty
    , archeResultView = Nothing
    , inputCfg = NoCfg
    , runningORProcesses = Dict.empty
    , runningArcheProcesses = Dict.empty
    }
  , Cmd.none
  )

defaultORCfg : ORConfig
defaultORCfg =
  { misoAngle    = { unDeg = 5.0 }
  , optByAvg     = False
  , predefinedOR = Nothing
  , startOR      = Nothing
  , parentPhase  = Right CubicPhase
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
  , parentPhase            = Right CubicPhase
  , productPhase           = {phaseId = 1, phaseSymm = CubicPhase}
  }

-- =========== UPDATE ===========
type Msg
  = SetToken String
  | ResetToken

  | RefreshEBSDs 
  | RefreshORs String
  | RefreshArches String String

  | CheckAsyncORs      String String
  | CheckAsyncArches   String String String
  | ReCheckAsyncORs    String String        (Result Http.Error OREval)
  | ReCheckAsyncArches String String String (Result Http.Error Arche)

  | SelectedEBSD String
  | SelectedOR String
  | SelectedArche String

  | SetORConfig ORConfig
  | SetArcheConfig ArcheCfg
  | SetUploadConfig Upload.Msg
  | ResetInput

  | SetResultMCL Float
  | SetResultType ResultType

  | SubmitORConfig ORConfig
  | SubmitArche ArcheCfg

  | ReceivedEBSDs                (Result Http.Error (Array EBSD))
  | ReceivedORs    String        (Result Http.Error (Array OREval))
  | ReceivedArches String String (Result Http.Error (Array Arche))

  | AsyncORProcess    String        (Result Http.Error String)
  | AsyncArcheProcess String String (Result Http.Error String)


inNsecs : Int -> Task.Task a ()
inNsecs secs = Process.sleep (1000.0 * toFloat secs)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RefreshEBSDs ->
      case model.token of
        Just tk -> (model, API.fetchEbsdList {token = tk} ReceivedEBSDs)
        _       -> (model, Cmd.none)

    SubmitORConfig orCfg ->
      case model.token of
        Just tk ->
          case ArcheTree.getEBSDFocusKey model.archeTree of
            Just ebsdHash -> (model, API.sendASyncORfit {token = tk, ebsdHash = ebsdHash} orCfg (AsyncORProcess ebsdHash))
            _             -> (model, Cmd.none)
        _             -> (model, Cmd.none)

    RefreshORs ebsdHash ->
      case model.token of
        Just tk -> (model, API.fetchORList {token = tk, ebsdHash = ebsdHash} (ReceivedORs ebsdHash))
        _       -> (model, Cmd.none)
  
    RefreshArches ebsdHash orHash ->
      case model.token of
        Just tk -> (model, API.fetchArcheList {token = tk, ebsdHash = ebsdHash, orHash = orHash} (ReceivedArches ebsdHash orHash))
        _       -> (model, Cmd.none)

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
   
    SetORConfig orCfg -> ({model | inputCfg = OCfg orCfg}, Cmd.none)
    
    ResetInput -> ({model | inputCfg = NoCfg}, Cmd.none)

    SetArcheConfig archeCfg -> ({model | inputCfg = ACfg archeCfg}, Cmd.none)

    SetResultMCL mcl -> ({model | archeResultView = Maybe.map (updateMcl mcl) model.archeResultView}, Cmd.none)
   
    SetResultType resTy -> ({model | archeResultView = Maybe.map (updateType resTy) model.archeResultView}, Cmd.none)

    -- ====================== OR fit async ======================
    AsyncORProcess hashE res -> case res of
      Err err  -> (model, Cmd.none)
      Ok hashO -> (
        { model | runningORProcesses = Dict.insert hashO {ebsdHash = hashE, count = 1} model.runningORProcesses},
        Cmd.batch [Task.perform (\_ -> CheckAsyncORs hashE hashO) (inNsecs 45)]
        )

    CheckAsyncORs hashE hashO ->
      let
        bump = Maybe.map (\v -> { v | count = v.count + 1})
        updated = Dict.update hashO bump model.runningORProcesses
      in case model.token of
        Just tk -> ({ model | runningORProcesses = updated}, API.fetchOR {token = tk, ebsdHash = hashE, orHash = hashO} (ReCheckAsyncORs hashE hashO))
        _       -> ({ model | runningORProcesses = updated}, Cmd.none)

    ReCheckAsyncORs hashE hashO res ->
      let
        recheck = case Dict.get hashO model.runningORProcesses of
          Just {ebsdHash, count} -> if (count < 5) && (ebsdHash == hashE)
            then Task.perform (\_ -> CheckAsyncORs hashE hashO) (inNsecs 30)
            else Cmd.none
          _ -> Cmd.none
      in case model.token of
        Just tk -> case res of
          Err (Http.BadStatus 404) -> (model, recheck)
          _                        -> (model, Cmd.none)
        _      -> (model, Cmd.none)

    -- ====================== OR Arche async ======================
    AsyncArcheProcess hashE hashO res -> case res of
      Err err  -> (model, Cmd.none)
      Ok hashA -> (
        { model | runningArcheProcesses = Dict.insert hashA {ebsdHash = hashE, orHash = hashO, count = 1} model.runningArcheProcesses},
        Cmd.batch [Task.perform (\_ -> CheckAsyncArches hashE hashO hashA) (inNsecs 45)]
        )

    CheckAsyncArches hashE hashO hashA ->
      let
        bump = Maybe.map (\v -> { v | count = v.count + 1})
        updated = Dict.update hashA bump model.runningArcheProcesses
      in case model.token of
        Just tk -> (
          { model | runningArcheProcesses = updated},
          API.fetchArche {token = tk, ebsdHash = hashE, orHash = hashO, archeHash = hashA} (ReCheckAsyncArches hashE hashO hashA)
          )
        _       -> ({ model | runningArcheProcesses = updated}, Cmd.none)

    ReCheckAsyncArches hashE hashO hashA res ->
      let
        recheck = case Dict.get hashA model.runningArcheProcesses of
          Just {ebsdHash, orHash, count} -> if (count < 5) && (ebsdHash == hashE) && (orHash == hashO)
            then Task.perform (\_ -> CheckAsyncArches hashE hashO hashA) (inNsecs 30)
            else Cmd.none
          _ -> Cmd.none
      in case model.token of
        Just tk -> case res of
          Err (Http.BadStatus 404) -> (model, recheck)
          _                        -> (model, Cmd.none)
        _      -> (model, Cmd.none)

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


    SubmitArche archeCfg ->
      case model.token of
        Just tk ->
          case ArcheTree.getEBSDFocusKey model.archeTree of
            Just ebsdHash -> case ArcheTree.getORFocusKey model.archeTree of
              Just orHash -> (model, API.sendASyncArche {token = tk, ebsdHash = ebsdHash, orHash = orHash} archeCfg (AsyncArcheProcess ebsdHash orHash))
              _             -> (model, Cmd.none)
            _             -> (model, Cmd.none)
        _             -> (model, Cmd.none)
    
    SetUploadConfig upmsg -> case getUploadModel model.inputCfg of
      Just upModel ->
        let
          (newModel, newCmd) = Upload.update upmsg upModel
        in if Upload.isDone newModel
          then
            ( { model | inputCfg = NoCfg }
            , Cmd.batch [Task.perform (\_ -> RefreshEBSDs) Time.now]
            )
          else
            ( {model | inputCfg = UCfg newModel }
            , Cmd.map SetUploadConfig newCmd
            )
      Nothing -> ({ model | inputCfg = UCfg <| Upload.initModelWithToken model.token } , Cmd.none)
    

-- =========== SUBSCRIPTIONS ===========
subscriptions : Model -> Sub Msg
subscriptions model = case getUploadModel model.inputCfg of
  Just upModel -> Sub.map SetUploadConfig (Upload.subscriptions upModel)
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
    
    inputs  = Just <| row columnShape [renderEBSDUpload model, renderORInput model, renderArcheInput model]
    tree    = Just <| row [Element.alignTop] base
    results = Maybe.map (renderResultExplorer msgBuilder) model.archeResultView
  in column
    [ Element.spacing 15
    , Element.alignTop
    ]
    (filterMaybes [inputs, tree, results])

columnShape : List (Element.Attribute msg)
columnShape =
  [ Element.centerX
  ]
renderEbsds : Model -> Element Msg
renderEbsds model =
  let
    cols = ArcheTree.listEBSDWithFocus model.archeTree renderEbsd
    input = renderEBSDTooglrUpload model
  in column
    [ Element.spacing 10
    , Element.padding 5
    , Element.width (Element.px 300)
    , Element.alignTop
    ] (input :: cols)

renderORs : Model -> Element Msg
renderORs model =
  let
    isActive = hasActiveInput model.inputCfg
    cols = ArcheTree.listORWithFocus model.archeTree (renderOREval isActive) 
    input = renderORToogleInput model
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
    input = renderArcheToogleInput model
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

renderOREval : Bool -> OREval -> Bool -> Element Msg
renderOREval isActive orEval isSelected = 
  let
    product = orEval.cfgOR.productPhase
    parenPhase = either (.phaseId >> Just) (\_ -> Nothing) orEval.cfgOR.parentPhase
    parenSymm = either .phaseSymm identity orEval.cfgOR.parentPhase

    activeAttrs = if isActive
      then [Element.transparent True, Element.alpha 0.5]
      else [Element.Events.onClick (SelectedOR orEval.hashOR)]
  in column
    (activeAttrs ++ boxShape isSelected)
    [ cardEnrty "avg. angular misfit" <| degToText orEval.resultOR.misfitError.avgError ++ "°"
    , cardEnrty "<100> <111> deviation" <| degToText orEval.resultOR.ksDeviation.planeDeviation ++ "°"
    , cardEnrty "<100> <111> deviation" <| degToText orEval.resultOR.ksDeviation.axisDeviation ++ "°"
    , cardEnrty "parent phase ID" <| intToText product.phaseId
    , cardEnrty "parent symmetry" <| symmToText product.phaseSymm
    , maybe Element.none (cardEnrty "product phase ID" << intToText) parenPhase
    , cardEnrty "product symmetry" <| symmToText parenSymm
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
      { onChange = \isAvg -> SetORConfig <| { orCfg | optByAvg = isAvg}
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
      { onChange = \deg -> SetORConfig <| { orCfg | misoAngle = {unDeg = deg}}
      , label = Input.labelAbove [] (text <| "Misorientation Angle = " ++ degToText orCfg.misoAngle ++ "°")
      , min = 0.1
      , max = 15
      , step = Just 0.1
      , value = orCfg.misoAngle.unDeg
      , thumb = Input.defaultThumb
      }
    phaseSel name orCfg getter setter = Input.radio
      radioShape
      { onChange = \phid -> SetORConfig <| (setter orCfg phid)
      , selected = getter orCfg
      , label = Input.labelAbove [] (text <| "Phase ID (" ++ name ++ ")")
      , options = maybe [] (.info >> .phases >> Array.toList >> List.map (\x -> Input.option x.numID (text x.name)))
               <| ArcheTree.getEBSDFocus model.archeTree
      }
 
    symmSel name orCfg getter setter = Input.radio
      radioShape
      { onChange = \symm -> SetORConfig <| (setter orCfg symm)
      , selected = getter orCfg
      , label = Input.labelAbove [] (text <| "Symmetry (" ++ name ++ ")")
      , options =
          [ Input.option HexagonalPhase (text "hcp")
          , Input.option CubicPhase     (text "bcc/fcc")
          ]
      }
  in case getORCfg model.inputCfg of
    Just orCfg -> column
      boxInputShape
      [ phaseSel "parent"  orCfg
          (.parentPhase >> either (.phaseId >> Just) (\_ -> Nothing))
          (\x phid -> {x | parentPhase = either (\y -> if y.phaseId == phid then Right y.phaseSymm else Left {y | phaseId = phid}) (\s -> Left {phaseId = phid, phaseSymm = s}) x.parentPhase })
      , symmSel  "parent"  orCfg
          (.parentPhase >> either .phaseSymm identity >> Just)
          (\x symm -> {x | parentPhase = either (\y -> Left {y | phaseSymm = symm}) (\_ -> Right symm) x.parentPhase })
      , phaseSel "product" orCfg
          (.productPhase >> .phaseId >> Just)
          (\x phid -> {x | productPhase = {phaseId = phid, phaseSymm = .phaseSymm x.productPhase}})
      , symmSel  "product" orCfg
          (.productPhase >> .phaseSymm >> Just)
          (\x symm -> {x | productPhase = {phaseId = .phaseId x.productPhase, phaseSymm = symm}})
      , isAvgCheckbox orCfg
      , misoSlider orCfg
      , submitButton (SubmitORConfig orCfg)
      ]
    Nothing -> Element.none

renderORToogleInput : Model -> Element Msg
renderORToogleInput model =
  case getORCfg model.inputCfg of
    Just orCfg -> toogleInput SetORConfig Nothing
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
      { onChange = \x -> SetArcheConfig <| { archeCfg | excludeFloatingGrains = x } 
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
      { onChange = \x -> SetArcheConfig <| { archeCfg | refinementSteps = Basics.round x } 
      , label = Input.labelAbove [] (text <| "Number of steps: " ++ intToText archeCfg.refinementSteps)
      , min = 1
      , max = 10
      , step = Just 1.0
      , value = toFloat archeCfg.refinementSteps
      , thumb = Input.defaultThumb
      }
  in case getArcheCfg model.inputCfg of
    Just archeCfg -> column
      boxInputShape
      [ excludeFloatingCheckbox archeCfg
      , stepsSlider archeCfg
      , submitButton (SubmitArche archeCfg)
      ]
    Nothing -> Element.none

renderArcheToogleInput : Model -> Element Msg
renderArcheToogleInput model =
  case getArcheCfg model.inputCfg of
    Just archeCfg -> toogleInput SetArcheConfig Nothing
    Nothing -> if ArcheTree.hasORFocus model.archeTree
      then toogleInput SetArcheConfig
        <| Just
        <| maybe defaultArcheCfg .cfgArche
        <| ArcheTree.getArcheFocus model.archeTree
      else Element.none
    
renderEBSDUpload : Model -> Element Msg
renderEBSDUpload model =
  case getUploadModel model.inputCfg of
    Just upModel -> Element.map SetUploadConfig <| Element.html (Upload.view upModel)
    Nothing -> Element.none

renderEBSDTooglrUpload : Model -> Element Msg
renderEBSDTooglrUpload model =
  case getUploadModel model.inputCfg of
    Just upModel -> 
      if Upload.isUploading upModel
        then Element.none
        else toogleInput SetUploadConfig Nothing
    Nothing -> toogleInput SetUploadConfig (Just Upload.Cancel)
  
-- =========== commum ============

radioShape : List (Element.Attribute msg)
radioShape = [Element.spacing 3, Element.padding 6]

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
toogleInput : (a -> Msg) -> Maybe a -> Element Msg
toogleInput func value =
  let
    color = case value of
      Just _ -> G.colorB
      _      -> G.colorB 
  in Input.button
    [ Font.size 20
    , Element.height (Element.px 30)
    , Element.centerX
    , Font.color <| color
    ]
    { onPress = Just (maybe ResetInput func value)
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
