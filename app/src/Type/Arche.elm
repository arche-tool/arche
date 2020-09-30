module Type.Arche exposing (..)

import Json.Decode as D
import Json.Encode as JE
import Array exposing (Array)

import Type.Texture exposing
  ( Deg
  , Phase
  , degDecoder
  , degEncoder
  , phaseDecoder
  , phaseEncoder
  )

type alias Arche =
  { hashArche : String
  , cfgArche  : ArcheCfg
  , results   : Array ArcheResult
  }   

type alias ArcheResult =
  { mclFactor : Float
  , parentIPF : PublicLink
  , errorMap  : PublicLink
  }

type alias PublicLink =
  { publicName : String
  , publicLink : String
  }

type alias ArcheCfg = 
  { misoAngle              : Deg
  , excludeFloatingGrains  : Bool
  , refinementSteps        : Int
  , initClusterFactor      : Float
  , stepClusterFactor      : Float
  , badAngle               : Deg
  , parentPhase            : Maybe Phase
  , productPhase           : Phase
  }

archeCfgEncoder : ArcheCfg -> JE.Value
archeCfgEncoder cfg =
  let
    ls =
      [ ("misoAngle", degEncoder cfg.misoAngle)
      , ("excludeFloatingGrains", JE.bool cfg.excludeFloatingGrains)
      , ("refinementSteps", JE.int cfg.refinementSteps)
      , ("initClusterFactor", JE.float cfg.initClusterFactor)
      , ("stepClusterFactor", JE.float cfg.stepClusterFactor)
      , ("badAngle", degEncoder cfg.badAngle)
      , ("parentPhase", maybeEncode phaseEncoder cfg.parentPhase)
      , ("productPhase", phaseEncoder cfg.productPhase)
      ]
    maybeEncode enc x = Maybe.withDefault JE.null <| Maybe.map enc x
  in JE.object <| List.filter (\x -> JE.null /= Tuple.second x) ls

archeCfgDecoder : D.Decoder ArcheCfg
archeCfgDecoder =
    D.map8 ArcheCfg
      (D.field "misoAngle" degDecoder)
      (D.field "excludeFloatingGrains" D.bool)
      (D.field "refinementSteps" D.int)
      (D.field "initClusterFactor" D.float)
      (D.field "stepClusterFactor" D.float)
      (D.field "badAngle" degDecoder)
      (D.field "parentPhase" <| D.maybe phaseDecoder)
      (D.field "productPhase" <| phaseDecoder)

archeDecoder : D.Decoder Arche
archeDecoder = D.map3 Arche
  (D.field "hashArche"  D.string)
  (D.field "cfgArche"   archeCfgDecoder)
  (D.field "results"    (D.array archeResultDecoder))

archeResultDecoder : D.Decoder ArcheResult
archeResultDecoder = D.map3 ArcheResult
  (D.field "mclFactor" D.float)
  (D.field "parentIPF" publicLinkDecoder)
  (D.field "errorMap"  publicLinkDecoder)

publicLinkDecoder : D.Decoder PublicLink 
publicLinkDecoder = D.map2 PublicLink
  (D.field "publicName" D.string)
  (D.field "publicLink" D.string)

archeListDecoder : D.Decoder (Array Arche)
archeListDecoder = D.array archeDecoder