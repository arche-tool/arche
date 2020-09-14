module Draw.Assembler exposing (
  compile,
  renderMesh
  )

import Array exposing (Array)
import Dict exposing (Dict)
import Maybe exposing (Maybe)
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (Vec4, vec4)
import Math.Matrix4 as M4
import WebGL as GL
import WebGL.Settings exposing (cullFace, back, front)
import WebGL.Settings.DepthTest as DepthTest

import Draw.Types exposing (..)
import Draw.Shaders as Shaders

renderMesh : CameraInfo -> Mesh -> GL.Entity
renderMesh camera mesh =
    let
        modelM = M4.makeTranslate (vec3 -1 0 0)
        uniforms =
            { modelViewProjectionMatrix = M4.mul camera.viewProjection modelM
            , modelMatrix = modelM
            , viewPosition = camera.position
            }
    in  GL.entityWith [ DepthTest.default ] Shaders.vertexShader Shaders.fragmentShader mesh uniforms

compile : MicroStructure -> VoxelInfo Color -> Mesh
compile micro voxelInfo =
  let
    (MicroStructure {voxels}) = micro
    nullColor = vec4 0.0 0.0 0.0 0.0
    ts = Array.indexedMap (\i v -> renderCube micro (Maybe.withDefault nullColor <| Dict.get i voxelInfo) v) voxels
  in GL.triangles (List.concat <| Array.toList ts)

{-
face struct -> (((rtf, rbf), (ltf, lbf)), ((rtb, rbb), (ltb, lbb)))
legend -> [rigth|left][front|back][top|bottom] e.g. rft
       -> [right|left] <=> X
       -> [top|bottom] <=> Y
       -> [front|back] <=> Z
-}
renderCube : MicroStructure -> Color -> (((Int, Int), (Int, Int)), ((Int, Int), (Int, Int))) -> List (Vertex , Vertex, Vertex)
renderCube (MicroStructure {xVertices, yVertices, zVertices}) color (((rtf, rbf), (ltf, lbf)), ((rtb, rbb), (ltb, lbb))) =
  let
    getX i = Maybe.withDefault 0.0 (Array.get i xVertices) 
    getY i = Maybe.withDefault 0.0 (Array.get i yVertices) 
    getZ i = Maybe.withDefault 0.0 (Array.get i zVertices) 
    v_rtf = vec3 (getX rtf) (getY rtf) (getZ rtf)
    v_rbf = vec3 (getX rbf) (getY rbf) (getZ rbf)
    v_ltf = vec3 (getX ltf) (getY ltf) (getZ ltf)
    v_lbf = vec3 (getX lbf) (getY lbf) (getZ lbf)
    v_rtb = vec3 (getX rtb) (getY rtb) (getZ rtb)
    v_rbb = vec3 (getX rbb) (getY rbb) (getZ rbb)
    v_ltb = vec3 (getX ltb) (getY ltb) (getZ ltb)
    v_lbb = vec3 (getX lbb) (getY lbb) (getZ lbb)
    in List.concat <|
        [ renderFace color v_rtf v_rbf v_lbf v_ltf -- Z+
        , renderFace color v_rtb v_rbb v_lbb v_ltb -- Z-
        , renderFace color v_rtf v_ltf v_ltb v_rtb -- Y+
        , renderFace color v_rbf v_lbf v_lbb v_rbb -- Y-
        , renderFace color v_rbf v_rbb v_rtb v_rtf -- X+
        , renderFace color v_lbf v_lbb v_ltb v_ltf -- X+
        ]

renderFace : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
renderFace color a b c d =
  [ renderTriangle color a b c 
  , renderTriangle color c d a
  ]

renderTriangle : Color -> Vec3 -> Vec3 -> Vec3 -> ( Vertex, Vertex, Vertex )
renderTriangle color a b c =
  let
    centerX = vec3 1 0 0
    centerY = vec3 0 1 0
    centerZ = vec3 0 0 1
    vertex pos center = {position = pos, faceBaricenter = center, color = color}
  in ( vertex a centerX, vertex b centerY, vertex c centerZ)

