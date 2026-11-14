module Type.EBSD exposing (..)

import Json.Decode as D
import Array exposing (Array)
import Type.Texture exposing (tuple2Fdec)

type alias EBSD =
  { alias: String
  , hashEBSD: String
  , createdBy: User
  , info: EBSDmeta
  }

type alias User =
  { id_number: String
  , email: Maybe String
  , name: Maybe String
  }

type alias EBSDphase =
  { numID : Int
  , name  : String
  }

type alias EBSDmeta = 
  { rows : Int
  , cols : Int
  , xystep : (Float, Float)
  , phases : Array EBSDphase
  }

userDecoder : D.Decoder User
userDecoder =
    D.map3 User
      (D.field "id_number" D.string)
      (D.field "name" (D.nullable D.string))
      (D.field "email" (D.nullable D.string))

ebsdDecoder : D.Decoder EBSD
ebsdDecoder =
    D.map4 EBSD
      (D.field "alias" D.string)
      (D.field "hashEBSD" D.string)
      (D.field "createdBy" userDecoder)
      (D.field "info" ebsdMetaDecoder)

ebsdMetaDecoder : D.Decoder EBSDmeta
ebsdMetaDecoder =
    D.map4 EBSDmeta
      (D.field "rows" D.int)
      (D.field "cols" D.int)
      (D.field "xystep" tuple2Fdec)
      (D.field "phases" (D.array ebsdPhaseDecoder))

ebsdPhaseDecoder : D.Decoder EBSDphase
ebsdPhaseDecoder =
    D.map2 EBSDphase
      (D.field "numID" D.int)
      (D.field "name" D.string)

ebsdListDecoder : D.Decoder (Array EBSD)
ebsdListDecoder = D.array ebsdDecoder