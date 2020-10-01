module Utils exposing
    ( floatToText
    , intToText
    , degToText
    , symmToText
    , maybe
    , either
    , filterMaybes
    )

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), base)

import Type.Texture exposing (Deg, PhaseSymm(..), Either(..))

-- =========== Formatters ===========
floatToText : Float -> String
floatToText = format {base | decimals = Exact 1}

intToText : Int -> String
intToText = format {base | decimals = Exact 0} << toFloat

degToText : Deg -> String
degToText v = format {base | decimals = Exact 1} v.unDeg

maybe : b -> (a -> b) -> Maybe a -> b
maybe def func x = Maybe.withDefault def (Maybe.map func x)

either : (a -> c) -> (b -> c) -> Either a b -> c
either fa fb e = case e of
   Left a  -> fa a
   Right b -> fb b

filterMaybes : List (Maybe a) -> List a
filterMaybes l = case l of
    (Just x)::ms -> x::filterMaybes ms 
    Nothing::ms  -> filterMaybes ms 
    _            -> []

symmToText : PhaseSymm -> String
symmToText v = case v of
    CubicPhase     -> "Cubic"
    HexagonalPhase -> "Hexagonal"