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
  , uploadInput: Upload.Model
  , orCfgInput: ORConfig
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    (upModel, _) = Upload.init ()
  in
    ( { token = Nothing
      , archeTree = ArcheTree.empty
      , archeResultView = Nothing
      , uploadInput = upModel
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
  | RefreshArches String String

  | SelectedEBSD String
  | SelectedOR String
  | SelectedArche String
  | SetORConfig ORConfig
  | SetResultMCL Float

  | SubmitORConfig ORConfig
  | SubmitArche ArcheCfg

  | ReceivedEBSDs                (Result Http.Error (Array EBSD))
  | ReceivedORs    String        (Result Http.Error (Array OREval))
  | ReceivedArches String String (Result Http.Error (Array Arche))

  | NewOR String (Result Http.Error OREval)
  | Upload Upload.Msg

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
        , Task.perform (\_ -> Upload (Upload.SetToken tk)) Time.now
        ]
      )
    
    ResetToken -> ({model | token = Nothing}, Cmd.none)
   
    SetORConfig orCfg -> ({model | orCfgInput = orCfg}, Cmd.none)

    SetResultMCL mcl -> ({model | archeResultView = Maybe.map (selectResult mcl) model.archeResultView}, Cmd.none)
    
    NewOR _ _ -> (model, Cmd.none)

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
                   , expect = Http.expectJson (NewOR ebsdHash) orEvalDecoder
                   , timeout = Nothing
                   , tracker = Nothing
                   }
                   )
              _ -> (model, Cmd.none)
            _ -> (model, Cmd.none)
        _ -> (model, Cmd.none)
    
    Upload upmsg ->
      let
        (newModel, newCmd) = Upload.update upmsg model.uploadInput
      in ( {model | uploadInput = newModel }, Cmd.map Upload newCmd )
    

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
      , renderArches model.archeTree
      ]
    rv = maybe [] (List.singleton << renderResultExplorer) model.archeResultView
  in row [] (base ++ rv)

renderEbsds : Model -> Element Msg
renderEbsds model =
  let
    cols = ArcheTree.listEBSDWithFocus model.archeTree renderEbsd
    input = renderEBSDUpload model.uploadInput
  in column
    [ Element.spacing 10
    , Element.padding 5
    ] (input :: cols)

renderORs : Model -> Element Msg
renderORs model =
  let
    cols = ArcheTree.listORWithFocus model.archeTree renderOREval 
    input = renderORInput model.orCfgInput False
  in column
    [ Element.spacing 10
    , Element.padding 5
    ] (input :: cols)

renderArches :  ArcheTree -> Element Msg
renderArches at =
  let
    cols = ArcheTree.listArchesWithFocus at renderArche 
    input = renderArcheInput testArche
  in column
    [ Element.spacing 10
    , Element.padding 5
    ] (input :: cols)

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

renderArche : Arche -> Bool -> Element Msg
renderArche arche isSelected =
  let
    attrs =
      [ Element.Events.onClick (SelectedArche arche.hashArche)
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
    elms = 
      [ text (String.fromInt arche.cfgArche.refinementSteps)
      , text arche.hashArche
      , Input.button
        [ BG.color blue
        , Element.focused
          [ BG.color purple ]
        ]
        { onPress = Just (SubmitArche testArche)
        , label = text "+"
        }
      ]
  in column attrs elms

testArche = 
  { misoAngle              = Deg 5.0 
  , excludeFloatingGrains  = True
  , refinementSteps        = 5
  , initClusterFactor      = 1.25
  , stepClusterFactor      = 1.2
  , badAngle               = Deg 15.0
  , parentPhaseID          = Nothing
  }


renderResultExplorer : ArcheResultExplorer -> Element Msg
renderResultExplorer resultExplorer =
  let
    attrs =
      [ Element.Border.rounded 3
      , Element.padding 3
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

renderArcheInput : ArcheCfg -> Element Msg
renderArcheInput archeCfg =
  let
    isAvgCheckbox = Input.checkbox []
      { onChange = \_ -> RefreshEBSDs
      , icon = Input.defaultCheckbox
      , checked = True
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
      { onChange = \_ -> RefreshEBSDs
      , label = Input.labelAbove [] (text <| "Misorientation Angle")
      , min = 0.1
      , max = 15
      , step = Just 0.1
      , value = 0.0
      , thumb = Input.defaultThumb
      }
    submitNewOR = Input.button
      [ BG.color blue
      , Element.focused
          [ BG.color purple ]
      ]
      { onPress = Just (SubmitArche archeCfg)
      , label = text "+"
      }
  in column
  [ Element.Border.rounded 3
  , Element.padding 10
  , Element.spacing 10
  , Element.pointer
  , BG.color (rgb255 200 100 100)
  , Element.htmlAttribute
    (Html.Attributes.style "user-select" "none")
  ]
  [ isAvgCheckbox
  , misoSlider
  , submitNewOR
  ]

renderEBSDUpload : Upload.Model -> Element Msg
renderEBSDUpload archeCfg = column
  [ Element.Border.rounded 3
  , Element.padding 10
  , Element.spacing 10
  , Element.pointer
  , BG.color (rgb255 200 100 100)
  , Element.htmlAttribute
    (Html.Attributes.style "user-select" "none")
  ]
  [ Element.map Upload <| Element.html (Upload.view archeCfg)
  ]



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