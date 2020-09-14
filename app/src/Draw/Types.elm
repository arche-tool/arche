module Draw.Types exposing (
    MicroStructure(..),
    VoxelInfo,
    Color,
    Mesh,
    Vertex,
    Uniforms,
    CameraInfo
    )

import Dict exposing (Dict)
import Array exposing (Array)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (Mat4)
import WebGL as GL

type alias Mesh = GL.Mesh Vertex

type alias Vertex =
    { position : Vec3
    , faceBaricenter : Vec3
    , color : Color
    }

type alias Color = Vec4

type MicroStructure
    = MicroStructure
    { xVertices : Array Float
    , yVertices : Array Float
    , zVertices : Array Float
    , voxels : Array (((Int, Int), (Int, Int)), ((Int, Int), (Int, Int)))
    }

type alias VoxelInfo a = Dict Int a

type alias Uniforms = 
    { modelViewProjectionMatrix : Mat4
    , modelMatrix : Mat4
    , viewPosition : Vec3
    }

type alias CameraInfo =
    { projection : Mat4
    , view : Mat4
    , viewProjection : Mat4
    , position : Vec3
    }
