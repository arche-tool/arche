module Widget.ArcheResultExplorer exposing
    ( ArcheResultExplorer
    , Courier
    , renderResultExplorer
    , newResult
    , selectResult 
    )

import Array exposing (Array)
import Html.Attributes

import Element exposing (Element, column, text)
import Element.Background as BG
import Element.Border
import Element.Input as Input

import Type.Arche exposing (Arche, ArcheResult)
import Globals as G
import Utils exposing (..)

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


type alias Courier a =
    { selectedMcl : Float -> a
    }

renderResultExplorer : Courier a -> ArcheResultExplorer -> Element a
renderResultExplorer courier resultExplorer =
  let
    attrs =
      [ Element.Border.rounded 3
      , Element.padding 5
      , Element.spacing 5
      , Element.pointer
      , Element.centerX
      , BG.color G.colorA
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
      ]
  in column attrs (imgs ++ resultSlider)
