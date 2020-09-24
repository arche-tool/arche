module Type.Texture exposing
  ( Deg
  , AxisPair
  , PhaseID
  , Symm(..)
  , degEncoder
  , axisPairEncoder
  , degDecoder
  , axisPairDecoder
  , symmEncoder
  , symmPhaseEncoder
  , phaseIdEncoder
  , symmDecoder
  , symmPhaseDecoder 
  , phaseIdDecoder
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

type Symm
    = Hexagonal
    | Cubic

type alias PhaseID = Int

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

symmEncoder : Symm -> JE.Value
symmEncoder s =
  let
    core = case s of
      Hexagonal -> JE.string "Hexgonal"
      Cubic     -> JE.string "Cubic"
  in JE.object [("tag", core)] 

phaseIdEncoder : PhaseID -> JE.Value
phaseIdEncoder phaseId = JE.object
  [ ("phaseId", JE.int phaseId) ]

symmPhaseEncoder : (PhaseID, Symm) -> JE.Value
symmPhaseEncoder (ph, sy) = JE.list identity [phaseIdEncoder ph, symmEncoder sy]

degDecoder : D.Decoder Deg
degDecoder = D.map Deg (D.field "unDeg" D.float)

axisPairDecoder : D.Decoder AxisPair
axisPairDecoder = D.field "axisAngle" (D.map2 AxisPair tuple3Fdec D.float)

symmDecoder : D.Decoder Symm
symmDecoder =
  let
    match str = case str of
      "Cubic"     -> D.succeed Cubic
      "Hexagonal" -> D.succeed Hexagonal
      some        -> D.fail <| "Unknown Symmetry: " ++ some
  in D.field "tag" (D.string |> D.andThen match)

phaseIdDecoder : D.Decoder PhaseID
phaseIdDecoder = D.field "phaseId" D.int

symmPhaseDecoder : D.Decoder (PhaseID, Symm)
symmPhaseDecoder = D.map2 (\a b -> (a, b)) (D.index 0 phaseIdDecoder) (D.index 1 symmDecoder)

tuple3Fdec : D.Decoder (Float, Float, Float)
tuple3Fdec = D.map3 (\a b c -> (a,b,c)) (D.index 0 D.float) (D.index 1 D.float) (D.index 2 D.float)

tuple2Fdec : D.Decoder (Float, Float)
tuple2Fdec = D.map2 (\a b -> (a,b)) (D.index 0 D.float) (D.index 1 D.float)

tuple3Idec : D.Decoder (Int, Int, Int)
tuple3Idec = D.map3 (\a b c -> (a,b,c)) (D.index 0 D.int) (D.index 1 D.int) (D.index 2 D.int)