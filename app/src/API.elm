module API exposing (..)
import Http
import Array exposing (Array)

import Json.Decode as D
import Globals as G

import Type.EBSD exposing (
  EBSD,
  ebsdDecoder,
  ebsdListDecoder
  )

import Type.OR exposing (
  OREval,
  ORConfig,
  orCfgEncoder,
  orEvalDecoder,
  orEvalListDecoder
  )

import Type.Arche exposing (
  Arche,
  ArcheCfg, 
  archeCfgEncoder,
  archeDecoder,
  archeListDecoder
  )
import Html exposing (param)




fetchArche : { token : String, ebsdHash : String, orHash : String, archeHash : String } -> (Result Http.Error Arche -> a) -> Cmd a
fetchArche param func =  
    let
        hs = [Http.header "Authorization" ("Bearer " ++ param.token)]
    in Http.request
        { method = "GET"
        , url = "/api/ebsd/hash/" ++ param.ebsdHash ++ "/orfit/hash/" ++ param.orHash ++ "/arche/hash/" ++ param.archeHash
        , headers = hs
        , body = Http.emptyBody
        , expect = Http.expectJson func archeDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

fetchArcheList : { token : String, ebsdHash : String, orHash : String } -> (Result Http.Error (Array Arche) -> a) -> Cmd a
fetchArcheList param func =  
    let
        hs = [Http.header "Authorization" ("Bearer " ++ param.token)]
    in Http.request
        { method = "GET"
        , url = "/api/ebsd/hash/" ++ param.ebsdHash ++ "/orfit/hash/" ++ param.orHash ++ "/arche"
        , headers = hs
        , body = Http.emptyBody
        , expect = Http.expectJson func archeListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

fetchOR : { token : String, ebsdHash : String, orHash : String } -> (Result Http.Error OREval -> a) -> Cmd a
fetchOR param func =  
    let
        hs = [Http.header "Authorization" ("Bearer " ++ param.token)]
    in Http.request
        { method = "GET"
        , url = "/api/ebsd/hash/" ++ param.ebsdHash ++ "/orfit/hash/" ++ param.orHash
        , headers = hs
        , body = Http.emptyBody
        , expect = Http.expectJson func orEvalDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

fetchORList : { token : String, ebsdHash : String } -> (Result Http.Error (Array OREval) -> a) -> Cmd a
fetchORList param func =  
    let
        hs = [Http.header "Authorization" ("Bearer " ++ param.token)]
    in Http.request
        { method = "GET"
        , url = "/api/ebsd/hash/" ++ param.ebsdHash ++ "/orfit"
        , headers = hs
        , body = Http.emptyBody
        , expect = Http.expectJson func orEvalListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

fetchEbsd : { token : String, ebsdHash : String } -> (Result Http.Error EBSD -> a) -> Cmd a
fetchEbsd param func =  
    let
        hs = [Http.header "Authorization" ("Bearer " ++ param.token)]
    in Http.request
        { method = "GET"
        , url = "/api/ebsd/hash/" ++ param.ebsdHash
        , headers = hs
        , body = Http.emptyBody
        , expect = Http.expectJson func ebsdDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

fetchEbsdList : { token : String } -> (Result Http.Error (Array EBSD) -> a) -> Cmd a
fetchEbsdList param func =  
    let
        hs = [Http.header "Authorization" ("Bearer " ++ param.token)]
    in Http.request
        { method = "GET"
        , url = "/api/ebsd/"
        , headers = hs
        , body = Http.emptyBody
        , expect = Http.expectJson func ebsdListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

sendASyncORfit : { token : String, ebsdHash : String } -> ORConfig -> (Result Http.Error String -> a) -> Cmd a
sendASyncORfit param orCfg func =  
    let
        hs = [Http.header "Authorization" ("Bearer " ++ param.token)]
    in Http.request
        { method = "POST"
        , url = "/api/ebsd/hash/" ++ param.ebsdHash ++ "/orfit/async"
        , headers = hs
        , body = Http.jsonBody <| orCfgEncoder orCfg
        , expect = Http.expectJson func D.string
        , timeout = Nothing
        , tracker = Nothing
        }

sendASyncArche : { token : String, ebsdHash : String, orHash : String } -> ArcheCfg -> (Result Http.Error String -> a) -> Cmd a
sendASyncArche param archeCfg func =  
    let
        hs = [Http.header "Authorization" ("Bearer " ++ param.token)]
    in Http.request
        { method = "POST"
        , url = "/api/ebsd/hash/" ++ param.ebsdHash ++ "/orfit/hash/" ++ param.orHash ++ "/arche/async"
        , headers = hs
        , body = Http.jsonBody <| archeCfgEncoder archeCfg
        , expect = Http.expectJson func D.string
        , timeout = Nothing
        , tracker = Nothing
        }