module Page.Draw exposing (Model, Msg, main, init, update, subscriptions, view)

import Browser
import Browser.Events exposing (onMouseDown, onMouseMove, onMouseUp, onResize)
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events exposing (on, onInput)
import Json.Decode as JD exposing (int)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 exposing (vec3)
import Math.Matrix4 as M4
import WebGL as GL

import Draw.Types exposing (Mesh, CameraInfo)
import Draw.Assembler exposing (renderMesh)
import Draw.Loader exposing (loadMesh)

main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, initCmd )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

-- ================================== Model ===================================
type alias Model =
    { mesh : Result String Mesh
    , currentModel : String
    , zoom : Float
    , isDown : Bool
    , lastMousePos : Vec2
    , mouseDelta : Vec2
    , windowSize : Size
    }


init : Model
init =
    { mesh = loadMesh "testMesh" (\x -> x)
    , currentModel = "testMesh"
    , zoom = 5
    , isDown = False
    , lastMousePos = vec2 0 0
    , mouseDelta = vec2 0 (pi / 2)
    , windowSize = Size 800 600
    }

initCmd : Cmd Msg
initCmd = Cmd.batch []

-- ================================== Update ===================================
type alias Size = {
    width : Int,
    height : Int
    }

type Msg
    = LoadMesh String (Result String Mesh)
    | Zoom Float
    | MouseMove Int Int
    | MouseDown Int Int
    | MouseUp
    | ResizeWindow Int Int
    | SelectMesh String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Zoom dy ->
            ( { model | zoom = max 0.01 (model.zoom + dy / 100) }, Cmd.none )

        SelectMesh _ ->
            ( model, Cmd.none )

        LoadMesh _ _ ->
            ( model , Cmd.none )

        MouseDown x y ->
            ( { model | isDown = True, lastMousePos = vec2 (toFloat x) (toFloat y) }, Cmd.none )

        MouseUp ->
            ( { model | isDown = False }, Cmd.none )

        MouseMove x y ->
            let pos = vec2 (toFloat x) (toFloat y)
            in ( { model | mouseDelta = getDelta pos model.lastMousePos model.mouseDelta, lastMousePos = pos }, Cmd.none )

        ResizeWindow x y ->
            ( { model | windowSize = { width = x, height = y } }, Cmd.none )


-- ================================== Viewer ===================================
renderModel : Model -> Mesh -> GL.Entity
renderModel model mesh =
    let camera = getCamera model
    in renderMesh camera mesh

getCamera : Model -> CameraInfo
getCamera { mouseDelta, zoom, windowSize } =
    let
        ( mx, my ) = ( Vec2.getX mouseDelta, Vec2.getY mouseDelta )
        aspect   = toFloat windowSize.width / toFloat windowSize.height
        proj     = M4.makePerspective 45 aspect 0.01 10000
        position = vec3 (zoom * sin -mx * sin my) (-zoom * cos my + 1) (zoom * cos -mx * sin my)
        view_    = M4.makeLookAt position (vec3 0 1 0) (vec3 0 1 0)
    in
    { projection = proj, view = view_, viewProjection = M4.mul proj view_, position = position }

view : Model -> Html.Html Msg
view model =
    div []
        [ selectModel model
        , case model.mesh of
            Ok msh ->
                GL.toHtmlWith [ GL.antialias, GL.depth 1 ]
                    [ onZoom
                    , Attr.width model.windowSize.width
                    , Attr.height model.windowSize.height
                    ]
                    [renderModel model msh]

            Err m ->
                Html.div [] [ Html.text <| "ERROR with mesh: " ++ m ]

        ]

selectModel : Model -> Html Msg
selectModel model =
    div [ Attr.style "position" "absolute", Attr.style "z-index" "2", Attr.style "backgroundColor" "white" ]
        [ Html.select [ onInput SelectMesh, Attr.value model.currentModel ]
            (List.map (\t -> Html.option [ Attr.value t ] [ text t ]) [])
        ]
        

-- ================================== Subscriptions ===================================
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ((if model.isDown then
            [ onMouseMove (decodeMouse MouseMove) ]

          else
            []
         )
            ++ [ onMouseUp (JD.succeed MouseUp)
               , onMouseDown (decodeMouse MouseDown)
               , onResize ResizeWindow
               ]
        )

-- ================================== Utils ===================================
decodeMouse : (Int -> Int -> Msg) -> JD.Decoder Msg
decodeMouse mapper = JD.map2 mapper
    (JD.field "clientX" int)
    (JD.field "clientY" int)

onZoom : Html.Attribute Msg
onZoom = on "wheel" (JD.map Zoom (JD.field "deltaY" JD.float))

getDelta : Vec2 -> Vec2 -> Vec2 -> Vec2
getDelta curr lastP delta = vec2
    ((Vec2.getX curr - Vec2.getX lastP) / 100 + Vec2.getX delta)
    ((Vec2.getY curr - Vec2.getY lastP) / 100 + Vec2.getY delta |> clamp 0.01 pi)
