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
  archeListDecoder)

import Type.Texture exposing (Deg)

import Type.ArcheTree as ArcheTree
import Type.ArcheTree exposing (ArcheTree)
import Type.Arche exposing (ArcheResult)
import Page.Upload as Upload

-- =========== MAIN ===========
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = layout [] << view
    , update = update
    , subscriptions = subscriptions
    }

type alias ArcheResultExplorer =
  { minMclFactor: Float
  , maxMclFactor: Float
  , selectedResult: Maybe ArcheResult
  , selector: Float -> Maybe ArcheResult
  }

newResult : Arche -> ArcheResultExplorer
newResult arche =
  let
    arr = arche.results
    max = Array.foldl Basics.max 0 (Array.map (\x -> x.mclFactor) arr)
    min = Array.foldl Basics.min max (Array.map (\x -> x.mclFactor) arr)
  in
    { minMclFactor = min
    , maxMclFactor = max
    , selectedResult = Array.get 0 arr
    , selector = findClosestResult (\x -> x.mclFactor) arr
    }

findClosestResult : (a -> Float) -> Array a -> Float -> Maybe a 
findClosestResult func arr ref = Array.foldl (\x mvalue -> case mvalue of
    Just value ->
      let
        diffX = Basics.abs(func x - ref)
        diffV = Basics.abs(func value - ref)
      in if diffX < diffV then Just x else Just value
    Nothing -> Just x 
  ) Nothing arr

selectResult : Float -> ArcheResultExplorer -> ArcheResultExplorer
selectResult ref ae = {ae | selectedResult = ae.selector ref}

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
  let
    (upModel, _) = Upload.init ()
  in
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
  { misoAngle = { unDeg = 5.0 }
  , optByAvg = False
  , predefinedOR = Nothing
  }

defaultArcheCfg : ArcheCfg
defaultArcheCfg = 
  { misoAngle              = Deg 5.0 
  , excludeFloatingGrains  = True
  , refinementSteps        = 5
  , initClusterFactor      = 1.25
  , stepClusterFactor      = 1.2
  , badAngle               = Deg 15.0
  , parentPhaseID          = Nothing
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

  | Upload (Maybe Upload.Msg)
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
        , Task.perform (\_ -> Upload (Just <| Upload.SetToken tk)) Time.now
        ]
      )
    
    ResetToken -> ({model | token = Nothing}, Cmd.none)
   
    SetORConfig orCfg -> ({model | orCfgInput = orCfg}, Cmd.none)

    SetArcheConfig archeCfg -> ({model | archeCfgInput = archeCfg}, Cmd.none)

    SetResultMCL mcl -> ({model | archeResultView = Maybe.map (selectResult mcl) model.archeResultView}, Cmd.none)
    
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
    
    Upload Nothing      -> ({ model | uploadInput = Nothing }, Cmd.none)
    Upload (Just upmsg) -> case model.uploadInput of
      Just upModel ->
        let
          (newModel, newCmd) = Upload.update upmsg upModel
        in ( {model | uploadInput = Just newModel }, Cmd.map (Upload << Just) newCmd )
      Nothing -> ({ model | uploadInput = Just <| Upload.initModelWithToken model.token } , Cmd.none)
    

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
renderArcheTree model =
  let
    base =
      [ renderEbsds  model
      , renderORs    model
      , renderArches model
      ]
    rv = maybe [] (List.singleton << renderResultExplorer) model.archeResultView
  in row [] (base ++ rv)

renderEbsds : Model -> Element Msg
renderEbsds model =
  let
    cols = ArcheTree.listEBSDWithFocus model.archeTree renderEbsd
    input = renderEBSDUpload model
  in column
    [ Element.spacing 10
    , Element.padding 5
    , Element.width (Element.px 300)
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
    ] (input :: cols)

renderEbsd : EBSD -> Bool -> Element Msg
renderEbsd ebsd isSelected = column
  [ Element.Events.onClick (SelectedEBSD ebsd.hashEBSD)
  , Element.Border.rounded 3
  , Element.padding 3
  , Element.width Element.fill
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
  , Element.width Element.fill
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

renderArche : Arche -> Bool -> Element Msg
renderArche arche isSelected =
  let
    attrs =
      [ Element.Events.onClick (SelectedArche arche.hashArche)
      , Element.Border.rounded 3
      , Element.padding 3
      , Element.width Element.fill
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
    elms = 
      [ text (String.fromInt arche.cfgArche.refinementSteps)
      , text arche.hashArche
      ]
  in column attrs elms


renderResultExplorer : ArcheResultExplorer -> Element Msg
renderResultExplorer resultExplorer =
  let
    attrs =
      [ Element.Border.rounded 3
      , Element.padding 3
      , Element.width Element.fill
      , Element.pointer
      , BG.color (rgb255 100 200 100)
      , Element.htmlAttribute (Html.Attributes.style "user-select" "none")
      ]

    imgs = case resultExplorer.selectedResult of
      Just res -> [
        Element.image
          [ Element.width (Element.px 400) ]
          {src = res.parentIPF.publicLink, description = res.parentIPF.publicName}
        ]  
      Nothing -> []

    resultSlider =
      let
        value = case resultExplorer.selectedResult of
          Just mcl -> mcl.mclFactor 
          Nothing  -> resultExplorer.minMclFactor
        msg = case resultExplorer.selectedResult of
          Just mcl -> "MCL factor = " ++ floatToText mcl.mclFactor 
          Nothing  -> ""
      in [ Input.slider
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
        { onChange = SetResultMCL
        , label = Input.labelAbove [] (text <| msg)
        , min = resultExplorer.minMclFactor
        , max = resultExplorer.maxMclFactor
        , step = Just 0.1
        , value = value
        , thumb = Input.defaultThumb
        }
      ]
  in column attrs (imgs ++ resultSlider)


degToText : Deg -> String
degToText v = format {base | decimals = Exact 1} v.unDeg

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
              , BG.color (rgb255 120 120 120)
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
    submitNewOR orCfg = Input.button
      [ BG.color blue
      , Element.focused
          [ BG.color purple ]
      ]
      { onPress = Just (SubmitORConfig orCfg)
      , label = text "+"
      }
  in case model.orCfgInput of
    Just orCfg -> column
      [ Element.Border.rounded 3
      , Element.padding 10
      , Element.width Element.fill
      , Element.spacing 10
      , Element.pointer
      , BG.color (rgb255 100 200 100)
      , Element.htmlAttribute (Html.Attributes.style "user-select" "none")
      ]
      [ isAvgCheckbox orCfg
      , misoSlider orCfg
      , submitNewOR orCfg
      , toogleInput SetORConfig Nothing
      ]
    Nothing -> if ArcheTree.hasEBSDFocus model.archeTree
      then toogleInput SetORConfig
        <| Just
        <| maybe defaultORCfg .cfgOR
        <| ArcheTree.getORFocus model.archeTree
      else Element.none

toogleInput : (Maybe a -> msg) -> Maybe a -> Element msg
toogleInput func value = Input.button []
  { onPress = Just (func value)
  , label = text <| case value of
    Just _ -> "+"
    _      -> "-" 
  }

renderArcheInput : Model -> Element Msg
renderArcheInput model =
  let
    isAvgCheckbox = Input.checkbox []
      { onChange = \_ -> RefreshEBSDs
      , icon = Input.defaultCheckbox
      , checked = True
      , label = Input.labelRight [] (text "Use avg orientation?")
      }

    misoSlider archeCfg = Input.slider
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
      { onChange = \x -> SetArcheConfig <| Just { archeCfg | refinementSteps = Basics.round x } 
      , label = Input.labelAbove [] (text <| "Misorientation Angle")
      , min = 1
      , max = 15
      , step = Just 1.0
      , value = toFloat archeCfg.refinementSteps
      , thumb = Input.defaultThumb
      }
    submitNewOR archeCfg = Input.button
      [ BG.color blue
      , Element.focused
          [ BG.color purple ]
      ]
      { onPress = Just (SubmitArche archeCfg)
      , label = text "+"
      }
  in case model.archeCfgInput of
    Just archeCfg -> column
      [ Element.Border.rounded 3
      , Element.padding 10
      , Element.width Element.fill
      , Element.spacing 10
      , Element.pointer
      , BG.color (rgb255 200 100 100)
      , Element.htmlAttribute
        (Html.Attributes.style "user-select" "none")
      ]
      [ isAvgCheckbox
      , misoSlider archeCfg
      , submitNewOR archeCfg
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
      [ Element.Border.rounded 3
      , Element.padding 10
      , Element.width Element.fill
      , Element.spacing 10
      , Element.pointer
      , BG.color (rgb255 200 100 100)
      , Element.htmlAttribute (Html.Attributes.style "user-select" "none")
      ]
      [ Element.map (Upload << Just) <| Element.html (Upload.view upModel)
      , if Upload.isUploading upModel
        then Element.none
        else toogleInput Upload Nothing
      ]
    Nothing -> toogleInput Upload (Just Upload.Cancel)

-- =========== Colors ========
blue : Color
blue = Element.rgb255 238 238 238

purple : Color
purple = Element.rgb255 138 138 238

insurelloBlue : Element.Color
insurelloBlue = rgb255 59 139 186

-- =========== Formatters ===========

floatToText : Float -> String
floatToText v = format {base | decimals = Exact 1} v

maybe : b -> (a -> b) -> Maybe a -> b
maybe def func x = Maybe.withDefault def (Maybe.map func x)