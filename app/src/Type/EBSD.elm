module Type.EBSD exposing (..)

import Json.Decode as D
import Array exposing (Array)

type alias EBSD =
  { alias: String
  , hashEBSD: String
  , createdBy: User
  }

type alias User =
  { id_number: String
  , email: Maybe String
  , name: Maybe String
  }

userDecoder : D.Decoder User
userDecoder =
    D.map3 User
      (D.field "id_number" D.string)
      (D.field "name" (D.nullable D.string))
      (D.field "email" (D.nullable D.string))

ebsdDecoder : D.Decoder EBSD
ebsdDecoder =
    D.map3 EBSD
      (D.field "alias" D.string)
      (D.field "hashEBSD" D.string)
      (D.field "createdBy"userDecoder)

ebsdListDecoder : D.Decoder (Array EBSD)
ebsdListDecoder = D.array ebsdDecoder