module Widget.ArcheResultExplorer exposing
    ( ArcheResultExplorer
    , Courier
    , renderResultExplorer
    , newResult
    , updateMcl 
    , updateType 
    , ResultType (..)
    )

import Array exposing (Array)
import Html.Attributes

import Element exposing (Element, column, text, row)
import Element.Background as BG
import Element.Border
import Element.Input as Input

import Type.Arche exposing (Arche, ArcheResult, PublicLink)
import Globals as G
import Utils exposing (..)

type alias ArcheResultExplorer =
  { minMclFactor: Float
  , maxMclFactor: Float
  , selectedResult: Maybe (ArcheResult, ResultType)
  , selector: Float -> Maybe ArcheResult
  }

type ResultType = IPF | ErrorMap

newResult : Arche -> ArcheResultExplorer
newResult arche =
  let
    arr = arche.results
    max = Array.foldl Basics.max 0 (Array.map (\x -> x.mclFactor) arr)
    min = Array.foldl Basics.min max (Array.map (\x -> x.mclFactor) arr)
  in
    { minMclFactor = min
    , maxMclFactor = max
    , selectedResult = Maybe.map (\x -> (x, IPF)) (Array.get 0 arr)
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

updateType : ResultType -> ArcheResultExplorer -> ArcheResultExplorer
updateType ty ae = {ae | selectedResult = Maybe.map (\(x, _) -> (x, ty)) <| ae.selectedResult}

updateMcl : Float -> ArcheResultExplorer -> ArcheResultExplorer
updateMcl ref ae =
  let
    ty = case ae.selectedResult of
      Just (_, x) -> x
      _           -> IPF
  in {ae | selectedResult = Maybe.map (\x -> (x, ty)) <| ae.selector ref}



type alias Courier a =
    { selectedMcl : Float -> a
    , selectedType : ResultType -> a
    }

renderImg : ArcheResultExplorer -> Element a
renderImg resultExplorer = case resultExplorer.selectedResult of
  Just (res, ty) -> 
    let
      link = getLinkByType res ty
    in Element.image
      [ Element.width (Element.px 400) ]
      {src = link.publicLink, description = link.publicName}
  _ -> Element.none

getLinkByType : ArcheResult -> ResultType -> PublicLink
getLinkByType res ty = case ty of
   IPF -> res.parentIPF
   ErrorMap -> res.errorMap

renderSlider : Courier a -> ArcheResultExplorer -> Element a
renderSlider courier resultExplorer =
  let
    value = case resultExplorer.selectedResult of
      Just (mcl, _) -> mcl.mclFactor 
      Nothing       -> resultExplorer.minMclFactor
    msg = case resultExplorer.selectedResult of
      Just (mcl, _) -> "MCL factor = " ++ floatToText mcl.mclFactor 
      Nothing       -> ""
  in Input.slider
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
    { onChange = courier.selectedMcl
    , label = Input.labelAbove [] (text <| msg)
    , min = resultExplorer.minMclFactor
    , max = resultExplorer.maxMclFactor
    , step = Just 0.1
    , value = value
    , thumb = Input.defaultThumb
    }

renderTypeSelector : Courier a -> ArcheResultExplorer -> Element a
renderTypeSelector courier explorer = Input.radio
    [ Element.padding 10
    , Element.spacing 20
    ]
    { onChange = courier.selectedType
    , selected = Maybe.map Tuple.second explorer.selectedResult
    , label = Input.labelAbove [] (text "Result type")
    , options =
        [ Input.option IPF (text "IPF map")
        , Input.option ErrorMap (text "error map")
        ]
    }


renderResultExplorer : Courier a -> ArcheResultExplorer -> Element a
renderResultExplorer courier resultExplorer =
  let
    attrs =
      [ Element.Border.rounded 3
      , Element.padding 10
      , Element.spacing 10
      , Element.pointer
      , Element.centerX
      , BG.color G.colorA1
      , Element.htmlAttribute (Html.Attributes.style "user-select" "none")
      ]
    attrs_col =
      [ Element.spacing 10
      , Element.alignTop
      ]
  in row attrs
    [ column attrs_col
      [ renderTypeSelector courier resultExplorer
      ]
    , column attrs_col
      [ renderImg resultExplorer
      , renderSlider courier resultExplorer
      ]
    ]
