module Type.OR exposing (..)

import Json.Decode as D
import Json.Encode as JE
import Array exposing (Array)

type alias ORConfig =
  { misoAngle: Deg
  , optByAvg: Bool
  , predefinedOR: Maybe AxisPair
  }

type alias Deg =
    { unDeg: Float
    }
 
type alias AxisPair =
  { axis: (Float, Float, Float)
  , angle: Float
  }

orCfgEncoder : ORConfig -> JE.Value
orCfgEncoder cfg =
  let
    ls =
      [ ("misoAngle", degEncoder cfg.misoAngle)
      , ("optByAvg", JE.bool cfg.optByAvg)
      ] ++ Maybe.withDefault [] maybeAp 
    maybeAp = Maybe.map (\ap -> [("predefinedOR", axisPairEncoder ap)]) cfg.predefinedOR
  in JE.object ls

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

orCfgDecoder : D.Decoder ORConfig
orCfgDecoder =
    D.map3 ORConfig
      (D.field "misoAngle" degDecoder)
      (D.field "optByAvg" D.bool)
      (D.field "predefinedOR" <| D.maybe axisPairDecoder)

degDecoder : D.Decoder Deg
degDecoder = D.map Deg (D.field "unDeg" D.float)

axisPairDecoder : D.Decoder AxisPair
axisPairDecoder = D.field "axisAngle" (D.map2 AxisPair tuple3Fdec D.float)

tuple3Fdec : D.Decoder (Float, Float, Float)
tuple3Fdec = D.map3 (\a b c -> (a,b,c)) (D.index 0 D.float) (D.index 1 D.float) (D.index 2 D.float)

tuple3Idec : D.Decoder (Int, Int, Int)
tuple3Idec = D.map3 (\a b c -> (a,b,c)) (D.index 0 D.int) (D.index 1 D.int) (D.index 2 D.int)

type alias OREval =
  { cfgOR: ORConfig
  , resultOR: ResultOR
  , hashOR: String
  }

type alias ResultOR =
  { orientationRelationship: OrientationRelationship
  , ksDeviation: KSDeviation
  , misfitError: FitError
  }

type alias OrientationRelationship =
  { orAngle: Deg
  , orAxis: (Int, Int, Int)
  }

type alias KSDeviation =
  { planeDeviation: Deg
  , axisDeviation: Deg
  , directDeviation: Deg
  }

type alias FitError =
  { maxError: Deg
  , devError: Deg
  , avgError: Deg
  }

orEvalListDecoder : D.Decoder (Array OREval)
orEvalListDecoder = D.array orEvalDecoder

orEvalDecoder : D.Decoder OREval
orEvalDecoder =
    D.map3 OREval
      (D.field "cfgOR" orCfgDecoder)
      (D.field "resultOR" resultORDecoder)
      (D.field "hashOR" D.string)

resultORDecoder : D.Decoder ResultOR
resultORDecoder =
    D.map3 ResultOR
      (D.field "orientationRelationship" orientationRelationshipDecoder)
      (D.field "ksDeviation" ksDeviationDecoder)
      (D.field "misfitError" fitErrorDecoder)

orientationRelationshipDecoder : D.Decoder OrientationRelationship
orientationRelationshipDecoder =
    D.map2 OrientationRelationship
      (D.field "orAngle" degDecoder)
      (D.field "orAxis" tuple3Idec )

ksDeviationDecoder : D.Decoder KSDeviation
ksDeviationDecoder =
    D.map3 KSDeviation
      (D.field "planeDeviation" degDecoder)
      (D.field "axisDeviation" degDecoder)
      (D.field "directDeviation" degDecoder)

fitErrorDecoder : D.Decoder FitError
fitErrorDecoder =
    D.map3 FitError
      (D.field "maxError" degDecoder)
      (D.field "devError" degDecoder)
      (D.field "avgError" degDecoder)