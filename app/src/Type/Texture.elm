module Type.Texture exposing
  ( Deg
  , AxisPair
  , Phase
  , PhaseSymm(..)
  , Either(..)
  , degEncoder
  , degDecoder
  , axisPairEncoder
  , axisPairDecoder
  , symmEncoder
  , symmDecoder
  , phaseDecoder
  , phaseEncoder
  , eitherEncoder
  , eitherDecoder
  , eitherSymmPhaseEncoder
  , eitherSymmPhaseDecoder
  , tuple2Fdec
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

type PhaseSymm
    = HexagonalPhase
    | CubicPhase

type alias Phase =
  { phaseId : Int
  , phaseSymm : PhaseSymm
  }

type Either a b
    = Left a
    | Right b

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

symmEncoder : PhaseSymm -> JE.Value
symmEncoder s =
  let
    core = case s of
      HexagonalPhase -> JE.string "HexagonalPhase"
      CubicPhase     -> JE.string "CubicPhase"
  in core 

phaseEncoder : Phase -> JE.Value
phaseEncoder p = JE.object
  [ ("phaseId", JE.int p.phaseId)
  , ("phaseSymm", symmEncoder p.phaseSymm)
  ]

eitherEncoder : (a -> JE.Value) -> (b -> JE.Value) -> Either a b -> JE.Value
eitherEncoder aEnc bEnc x = case x of
   Left  a -> JE.object [("Left",  aEnc a)]
   Right b -> JE.object [("Right", bEnc b)]

eitherSymmPhaseEncoder : Either Phase PhaseSymm -> JE.Value
eitherSymmPhaseEncoder = eitherEncoder phaseEncoder symmEncoder

degDecoder : D.Decoder Deg
degDecoder = D.map Deg (D.field "unDeg" D.float)

axisPairDecoder : D.Decoder AxisPair
axisPairDecoder = D.field "axisAngle" (D.map2 AxisPair tuple3Fdec D.float)

symmDecoder : D.Decoder PhaseSymm
symmDecoder =
  let
    match str = case str of
      "CubicPhase"     -> D.succeed CubicPhase
      "HexagonalPhase" -> D.succeed HexagonalPhase
      some        -> D.fail <| "Unknown Symmetry: " ++ some
  in D.string |> D.andThen match

phaseDecoder : D.Decoder Phase
phaseDecoder = D.map2 Phase (D.field "phaseId" D.int) (D.field "phaseSymm" symmDecoder)

eitherDecoder : D.Decoder a -> D.Decoder b -> D.Decoder (Either a b)
eitherDecoder aDec bDec = D.oneOf
  [ D.map Left  (D.field "Left"  aDec)
  , D.map Right (D.field "Right" bDec)
  ]

eitherSymmPhaseDecoder : D.Decoder (Either Phase PhaseSymm)
eitherSymmPhaseDecoder = eitherDecoder phaseDecoder symmDecoder

tuple3Fdec : D.Decoder (Float, Float, Float)
tuple3Fdec = D.map3 (\a b c -> (a,b,c)) (D.index 0 D.float) (D.index 1 D.float) (D.index 2 D.float)

tuple2Fdec : D.Decoder (Float, Float)
tuple2Fdec = D.map2 (\a b -> (a,b)) (D.index 0 D.float) (D.index 1 D.float)

tuple3Idec : D.Decoder (Int, Int, Int)
tuple3Idec = D.map3 (\a b c -> (a,b,c)) (D.index 0 D.int) (D.index 1 D.int) (D.index 2 D.int)