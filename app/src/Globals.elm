
module Globals exposing (..)

import Element exposing (Element, Color, rgb255)
import Element.Border
import Element.Background as BG
import Element.Font as Font
import Element.Input as Input
import Html.Attributes

-- =========== Colors ========
black : Color
black = Element.rgb255 0 0 0

white : Color
white = rgb255 255 255 255

colorA : Color
colorA = rgb255 63 192 189 -- Main color #3fc0bd

colorA1 : Color
colorA1 = rgb255 63 130 192 -- Analogous color #3f82c0

colorA2 : Color
colorA2 = rgb255 63 192 125 -- Analogous color #3fc07d

colorB : Color
colorB = rgb255 192 63 66 -- Complementary color #c03f42

renderButton : List (Element.Attribute msg) -> String -> msg -> Element msg
renderButton extraAtrrs name x = Input.button
  (
    [ BG.color colorA2
    , Element.padding 5
    ] ++ extraAtrrs
  )
  { onPress = Just x
  , label = Element.text name
  }

boxInputShape : List (Element.Attribute msg)
boxInputShape =
  [ Element.Border.rounded 3
  , Element.padding 10
  , Element.width Element.fill
  , Element.spacing 10
  , Element.pointer
  , BG.color colorA1
  , Element.htmlAttribute (Html.Attributes.style "user-select" "none")
  ]

radioShape : List (Element.Attribute msg)
radioShape = [Element.spacing 3, Element.padding 6]

boxShape : Bool -> List (Element.Attribute msg)
boxShape isSelected =
  [ Element.Border.rounded 3
  , Element.padding 10
  , Element.spacing 3
  , Element.width Element.fill
  , Element.pointer
  , BG.color (if isSelected then colorA1 else colorA)
  , Font.color <| if isSelected then white else black
  , Element.htmlAttribute
    (Html.Attributes.style "user-select" "none")
  , Element.mouseOver
    [ Element.Border.color black
    , Element.Border.glow black 1
    , Element.Border.innerGlow black 1
    ]
  , Element.mouseDown [ Element.alpha 0.6 ]
  ]