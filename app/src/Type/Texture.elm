module Type.Texture exposing
  ( Deg
  , AxisPair
  , degEncoder
  , axisPairEncoder
  , degDecoder
  , axisPairDecoder
  , tuple3Idec
  , tuple3Fdec
  )

import Json.Decode as D
import Json.Encode as JE

type alias Deg =
    { unDeg: Float
    }
 
type alias AxisPair =
  { axis: (Float, Float, Float)
  , angle: Float
  }

degEncoder : Deg -> JE.Value
degEncoder deg = JE.object
  [ ("unDeg", JE.float deg.unDeg) ]

axisPairEncoder : AxisPair -> JE.Value
axisPairEncoder ap =
  let
    (x, y, z) = ap.axis
  in JE.object
      [ ("axisAngle", JE.list identity [JE.list JE.float [x, y, z], JE.float ap.angle])
    ] 

degDecoder : D.Decoder Deg
degDecoder = D.map Deg (D.field "unDeg" D.float)

axisPairDecoder : D.Decoder AxisPair
axisPairDecoder = D.field "axisAngle" (D.map2 AxisPair tuple3Fdec D.float)

tuple3Fdec : D.Decoder (Float, Float, Float)
tuple3Fdec = D.map3 (\a b c -> (a,b,c)) (D.index 0 D.float) (D.index 1 D.float) (D.index 2 D.float)

tuple3Idec : D.Decoder (Int, Int, Int)
tuple3Idec = D.map3 (\a b c -> (a,b,c)) (D.index 0 D.int) (D.index 1 D.int) (D.index 2 D.int)