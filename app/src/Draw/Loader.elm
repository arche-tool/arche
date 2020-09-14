module Draw.Loader exposing(
    loadMesh
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Http exposing (Response)
import Math.Vector4 as V4 exposing (Vec4, vec4)
import String exposing (fromInt)
import Task

import Draw.Assembler exposing (compile)
import Draw.Types exposing (..)

loadMesh : String -> (Result String Mesh -> msg) -> msg
loadMesh url msg =
    let
        -- face struct -> (((rtf, rbf), (ltf, lbf)), ((rtb, rbb), (ltb, lbb)))
        testMicro = MicroStructure
            { xVertices = Array.fromList [-1, -1,  1,  1, -1, -1,  1,  1, -1, -1,  1,  1]
            , yVertices = Array.fromList [ 1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1]
            , zVertices = Array.fromList [ 1,  1,  1,  1, -1, -1, -1, -1,  3,  3,  3,  3]
            , voxels = Array.fromList [(((0, 1), (2, 3)), ((4, 5), (6, 7))), (((0, 1), (2, 3)), ((8, 9), (10, 11)))]
            }
        mesh = compile testMicro (Dict.fromList [(0, vec4 1.0 0.0 0.0 1.0), (1, vec4 0.0 1.0 0.0 1.0)])
    in  msg (Result.fromMaybe "testError" (Just mesh))

httpErrorFor : Http.Error -> String
httpErrorFor e =
    case e of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus response ->
            "Failed"

        Http.BadBody err ->
            "Invalid request in " ++ err
