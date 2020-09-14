module Type.OR exposing (..)

import Json.Decode as D
import Json.Encode as JE
import Array exposing (Array)

import Type.Texture exposing
  ( Deg
  , AxisPair
  , degDecoder
  , degEncoder
  , axisPairDecoder
  , axisPairEncoder
  , tuple3Idec
  )

type alias ORConfig =
  { misoAngle: Deg
  , optByAvg: Bool
  , predefinedOR: Maybe AxisPair
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

orCfgDecoder : D.Decoder ORConfig
orCfgDecoder =
    D.map3 ORConfig
      (D.field "misoAngle" degDecoder)
      (D.field "optByAvg" D.bool)
      (D.field "predefinedOR" <| D.maybe axisPairDecoder)

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