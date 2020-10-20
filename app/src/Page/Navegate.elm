module Page.Navegate exposing (
  Model,
  Msg(..),
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
import Html as Html
import Html.Attributes as A
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

type Processing
  = UpProcess { uploadAlias: String, uploadModel: Upload.Model }
  | OrProcess { ebsdHash : String, orHash : String, cfg : ORConfig,  count : Int }
  | ArProcess { ebsdHash : String, orHash : String, archeHash : String, cfg : ArcheCfg,  count : Int }

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
  , runningProcesses: Dict String Processing
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { token = Nothing
    , archeTree = ArcheTree.empty
    , archeResultView = Nothing
    , inputCfg = NoCfg
    , runningProcesses = Dict.empty
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

  | AsyncORProcess    ORConfig String        (Result Http.Error String)
  | AsyncArcheProcess ArcheCfg String String (Result Http.Error String)


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
            Just ebsdHash -> (model, API.sendASyncORfit {token = tk, ebsdHash = ebsdHash} orCfg (AsyncORProcess orCfg ebsdHash))
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
    AsyncORProcess orCfg hashE res -> case res of
      Err err  -> (model, Cmd.none)
      Ok hashO ->
        let orProc = OrProcess {ebsdHash = hashE, orHash = hashO, cfg = orCfg,  count = 1}
        in (
          { model | runningProcesses = Dict.insert hashO orProc model.runningProcesses},
          Cmd.batch [Task.perform (\_ -> CheckAsyncORs hashE hashO) (inNsecs 45)]
          )

    CheckAsyncORs hashE hashO ->
      let
        bump x = case x of
          Just (OrProcess v) -> Just <| OrProcess { v | count = v.count + 1}
          _                  -> x
        updated = Dict.update hashO bump model.runningProcesses
      in case model.token of
        Just tk -> ({ model | runningProcesses = updated}, API.fetchOR {token = tk, ebsdHash = hashE, orHash = hashO} (ReCheckAsyncORs hashE hashO))
        _       -> ({ model | runningProcesses = updated}, Cmd.none)

    ReCheckAsyncORs hashE hashO res ->
      let
        recheck = case Dict.get hashO model.runningProcesses of
          Just (OrProcess {ebsdHash, count}) -> if (count < 5) && (ebsdHash == hashE)
            then Task.perform (\_ -> CheckAsyncORs hashE hashO) (inNsecs 30)
            else Cmd.none
          _ -> Cmd.none
      in case model.token of
        Just tk -> case res of
          Err (Http.BadStatus 404) -> (model, recheck)
          _                        -> (model, Cmd.none)
        _      -> (model, Cmd.none)

    -- ====================== OR Arche async ======================
    AsyncArcheProcess archeCfg hashE hashO res -> case res of
      Err err  -> (model, Cmd.none)
      Ok hashA ->
        let
          archeProc = ArProcess {ebsdHash = hashE, orHash = hashO, archeHash = hashA, cfg = archeCfg, count = 1} 
        in (
          { model | runningProcesses = Dict.insert hashA archeProc model.runningProcesses},
          Cmd.batch [Task.perform (\_ -> CheckAsyncArches hashE hashO hashA) (inNsecs 45)]
          )

    CheckAsyncArches hashE hashO hashA ->
      let
        bump x = case x of
          Just (ArProcess v) -> Just <| ArProcess { v | count = v.count + 1}
          _                  -> x
        updated = Dict.update hashA bump model.runningProcesses
      in case model.token of
        Just tk -> (
          { model | runningProcesses = updated},
          API.fetchArche {token = tk, ebsdHash = hashE, orHash = hashO, archeHash = hashA} (ReCheckAsyncArches hashE hashO hashA)
          )
        _       -> ({ model | runningProcesses = updated}, Cmd.none)

    ReCheckAsyncArches hashE hashO hashA res ->
      let
        recheck = case Dict.get hashA model.runningProcesses of
          Just (ArProcess {ebsdHash, orHash, count}) -> if (count < 5) && (ebsdHash == hashE) && (orHash == hashO)
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
              Just orHash -> (model, API.sendASyncArche {token = tk, ebsdHash = ebsdHash, orHash = orHash} archeCfg (AsyncArcheProcess archeCfg ebsdHash orHash))
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
    
    processes  = Just <| renderAllProcess model
    inputs  = Just <| row columnShape [renderEBSDUpload model, renderORInput model, renderArcheInput model]
    tree    = Just <| row [Element.alignTop] base
    results = Maybe.map (renderResultExplorer msgBuilder) model.archeResultView
  in column
    [ Element.spacing 15
    , Element.alignTop
    ]
    (filterMaybes [processes, inputs, tree, results])

renderEBSDProcess : Model -> Element Msg
renderEBSDProcess model =
  let
    es = []
  in Element.column columnShape2 es

renderAllProcess : Model -> Element Msg
renderAllProcess model =
  let
    ls = Dict.values model.runningProcesses
  in Element.column columnShape2 <| List.map (renderProcessing model) ls

renderProcessing : Model -> Processing -> Element Msg
renderProcessing model proc = case proc of
    OrProcess info ->
      let
        header = Element.text <| maybe "" .alias <| ArcheTree.findEBSD model.archeTree info.ebsdHash
        progressBar =  Element.html <|
          Html.progress
            [ A.value (String.fromInt info.count)
            , A.max "5"
            , A.style "display" "block"
            ] []
      in column G.boxInputShape <| header :: progressBar :: renderORConfig info.cfg
    ArProcess info ->
      let
        header = Element.text <| maybe "" .alias <| ArcheTree.findEBSD model.archeTree info.ebsdHash
        progressBar =  Element.html <|
          Html.progress
            [ A.value (String.fromInt info.count)
            , A.max "5"
            , A.style "display" "block"
            ] []
      in column G.boxInputShape <| header :: progressBar :: renderArcheConfig info.cfg
    UpProcess upModel -> Element.map SetUploadConfig <| Upload.renderProgress upModel.uploadModel.state


columnShape : List (Element.Attribute msg)
columnShape =
  [ Element.centerX
  ]

columnShape2 : List (Element.Attribute msg)
columnShape2 = 
  [ Element.spacing 10
  , Element.padding 5
  , Element.width (Element.px 300)
  , Element.alignTop
  ]

renderEbsds : Model -> Element Msg
renderEbsds model =
  let
    cols = ArcheTree.listEBSDWithFocus model.archeTree renderEbsd
    input = renderEBSDTooglrUpload model
  in column columnShape2 (input :: cols)

renderORs : Model -> Element Msg
renderORs model =
  let
    isActive = hasActiveInput model.inputCfg
    cols = ArcheTree.listORWithFocus model.archeTree (renderOREval isActive) 
    input = renderORToogleInput model
  in column columnShape2 (input :: cols)

renderArches :  Model -> Element Msg
renderArches model =
  let
    cols = ArcheTree.listArchesWithFocus model.archeTree renderArche 
    input = renderArcheToogleInput model
  in column columnShape2 (input :: cols)

renderEbsd : EBSD -> Bool -> Element Msg
renderEbsd ebsd isSelected =
  let
    (sx, sy) = ebsd.info.xystep
  in column
    (Element.Events.onClick (SelectedEBSD ebsd.hashEBSD) :: G.boxShape isSelected)
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
    (activeAttrs ++ G.boxShape isSelected) <|
    [ cardEnrty "avg. angular misfit" <| degToText orEval.resultOR.misfitError.avgError ++ "°"
    , cardEnrty "<100> <111> deviation" <| degToText orEval.resultOR.ksDeviation.planeDeviation ++ "°"
    , cardEnrty "<100> <111> deviation" <| degToText orEval.resultOR.ksDeviation.axisDeviation ++ "°"
    ] ++ renderORConfig orEval.cfgOR


renderORConfig : ORConfig -> List (Element Msg)
renderORConfig orCfg = 
  let
    product = orCfg.productPhase
    parenPhase = either (.phaseId >> Just) (\_ -> Nothing) orCfg.parentPhase
    parenSymm = either .phaseSymm identity orCfg.parentPhase
  in
    [ cardEnrty "parent phase ID" <| intToText product.phaseId
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

renderArcheConfig : ArcheCfg -> List (Element Msg)
renderArcheConfig archeCfg = 
  [ cardEnrty "steps" <| String.fromInt archeCfg.refinementSteps
  , cardEnrty "exclude floating grains" <| if archeCfg.excludeFloatingGrains then "☑" else "☐"
  , cardEnrty "initial cluster factor" <| floatToText archeCfg.initClusterFactor
  , cardEnrty "incremental cluster factor" <| floatToText archeCfg.stepClusterFactor
  , cardEnrty "angular misfit threshold" <| degToText archeCfg.badAngle
  ]

renderArche : Arche -> Bool -> Element Msg
renderArche arche isSelected =
  let
    attrs =
      Element.Events.onClick (SelectedArche arche.hashArche) :: G.boxShape isSelected
  in column attrs (renderArcheConfig arche.cfgArche)


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
      G.radioShape
      { onChange = \phid -> SetORConfig <| (setter orCfg phid)
      , selected = getter orCfg
      , label = Input.labelAbove [] (text <| "Phase ID (" ++ name ++ ")")
      , options = maybe [] (.info >> .phases >> Array.toList >> List.map (\x -> Input.option x.numID (text x.name)))
               <| ArcheTree.getEBSDFocus model.archeTree
      }
 
    symmSel name orCfg getter setter = Input.radio
      G.radioShape
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
      G.boxInputShape
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
      G.boxInputShape
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
    Just upModel -> Element.map SetUploadConfig <| Upload.renderInput upModel.state
    Nothing -> Element.none

renderEBSDTooglrUpload : Model -> Element Msg
renderEBSDTooglrUpload model =
  case getUploadModel model.inputCfg of
    Just upModel -> 
      if Upload.isUploading upModel
        then Element.none
        else toogleInput SetUploadConfig Nothing
    Nothing -> toogleInput SetUploadConfig (Just Upload.Cancel)
  
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
submitButton = G.renderButton [] "Submit"
