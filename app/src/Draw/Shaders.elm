module Draw.Shaders exposing (..)

import WebGL
import Math.Vector4 exposing (Vec4)
import Math.Vector3 exposing (Vec3)

import Draw.Types exposing (Vertex, Uniforms)

-- ================================ vertexShader ===================================
vertexShader : WebGL.Shader Vertex Uniforms { vBaricenter : Vec3, vColor : Vec4}
vertexShader = [glsl|
    attribute vec3 position;
    attribute vec3 faceBaricenter;
    attribute vec4 color;

    varying vec4 vColor;
    varying vec3 vBaricenter;

    uniform mat4 modelViewProjectionMatrix;
    uniform mat4 modelMatrix;
    uniform vec3 viewPosition;

    void main() {
        vec4 pos = vec4(position, 1.0);
        vec3 posWorld = (modelMatrix * pos).xyz;
        gl_Position = modelViewProjectionMatrix * pos;
        vColor = color;
        vBaricenter = faceBaricenter;
    }
|]


fragmentShader : WebGL.Shader {} Uniforms { vBaricenter : Vec3, vColor : Vec4 }
fragmentShader =
  [glsl|
    precision mediump float;
    
    varying vec4 vColor;
    varying vec3 vBaricenter;
    
    float edgeFactor(){
        vec3 a3 = smoothstep(vec3(0.0), vec3(0.01), vBaricenter);
        return min(min(a3.x, a3.y), a3.z);
    }

    void main () {
        gl_FragColor = mix(vec4(0.0, 0.0, 0.0, 1.0), vColor, edgeFactor());
    }
  |]