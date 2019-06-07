module Main exposing (main)

import AnimationFrame
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Keyboard exposing (KeyCode)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import PageVisibility exposing (Visibility(..))
import Set exposing (Set)
import Task
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture, defaultOptions)
import Window


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias KeyCodes =
    { down : KeyCode
    , enter : KeyCode
    , escape : KeyCode
    , left : KeyCode
    , right : KeyCode
    , space : KeyCode
    , up : KeyCode
    }


type alias Model =
    { action : String
    , direction : Direction
    , frames : List ( Int, Int )
    , pressed : Set KeyCode
    , state : State
    , texture : TextureData
    , window : Window.Size
    }


type Direction
    = Left
    | Right


type State
    = Paused
    | Playing Time


type TextureData
    = Initial
    | Success Texture
    | Error Error


frameRange : Int -> Int -> Int -> List ( Int, Int )
frameRange first last y =
    List.range first last |> List.map (\x -> ( x, y ))


frameValues : String -> List ( Int, Int )
frameValues key =
    Dict.fromList
        [ ( "action", frameRange 4 5 1 )
        , ( "all", frameRange 0 8 0 ++ frameRange 0 8 1 ++ frameRange 0 5 2 )
        , ( "back", frameRange 4 4 2 )
        , ( "duck", frameRange 3 3 0 )
        , ( "cheer", frameRange 7 8 0 )
        , ( "climb", frameRange 5 6 0 )
        , ( "fall", frameRange 2 2 0 )
        , ( "hang", frameRange 2 2 2 )
        , ( "hold", frameRange 2 3 1 )
        , ( "hurt", frameRange 4 4 0 )
        , ( "idle", frameRange 0 0 0 )
        , ( "jump", frameRange 1 1 0 )
        , ( "kick", frameRange 6 6 1 )
        , ( "skid", frameRange 3 3 2 )
        , ( "slide", frameRange 1 1 2 )
        , ( "stand", frameRange 5 5 2 )
        , ( "swim", frameRange 7 8 1 )
        , ( "talk", frameRange 0 0 2 )
        , ( "walk", frameRange 0 1 1 )
        ]
        |> Dict.get key
        |> Maybe.withDefault []


init : ( Model, Cmd Msg )
init =
    ( { action = "stand"
      , direction = Right
      , frames = frameValues "stand"
      , pressed = Set.empty
      , state = Playing 0
      , texture = Initial
      , window = Window.Size 0 0
      }
    , Cmd.batch
        [ Task.attempt LoadTexture
            (Texture.loadWith
                { defaultOptions | flipY = False }
                "../sheets/soldier.png"
            )
        , Task.perform Resize Window.size
        ]
    )


keyCodes : KeyCodes
keyCodes =
    { down = 40
    , enter = 13
    , escape = 27
    , left = 37
    , right = 39
    , space = 32
    , up = 38
    }



-- UPDATE


type Msg
    = Animate Time
    | KeyOn KeyCode
    | KeyOff KeyCode
    | LoadTexture (Result Error Texture)
    | Resize Window.Size
    | VisibilityChange Visibility


animate : Time -> Model -> Model
animate delta model =
    case model.state of
        Paused ->
            model

        Playing time ->
            let
                timeout =
                    200

                updatedTime =
                    (min delta 60) + time
            in
                if updatedTime > timeout then
                    { model
                        | frames = rotateFrames model.frames
                        , state = Playing (updatedTime - timeout)
                    }
                else
                    { model | state = Playing updatedTime }


keyOn : KeyCode -> Model -> Model
keyOn code model =
    let
        { down, enter, escape, left, right, space, up } =
            keyCodes

        newModel =
            { model | pressed = Set.insert code model.pressed }

        pressed code =
            Set.member code model.pressed
    in
        if not (pressed code) then
            if code == escape then
                case model.state of
                    Paused ->
                        { newModel | state = Playing 0 }

                    Playing _ ->
                        { newModel | state = Paused }
            else if code == left then
                if not (pressed right) then
                    { newModel | direction = Left }
                else
                    model
            else if code == right then
                if not (pressed left) then
                    { newModel | direction = Right }
                else
                    model
            else
                newModel
        else
            model


move : Model -> Model
move model =
    let
        { down, left, right, up } =
            keyCodes

        pressed code =
            Set.member code model.pressed
    in
        if pressed left || pressed right then
            if model.action /= "walk" then
                { model | action = "walk", frames = frameValues "walk" }
            else
                model
        else if pressed up then
            if model.action /= "back" then
                { model | action = "back", frames = frameValues "back" }
            else
                model
        else if pressed down then
            if model.action /= "duck" then
                { model | action = "duck", frames = frameValues "duck" }
            else
                model
        else if model.action /= "stand" then
            { model | action = "stand", frames = frameValues "stand" }
        else
            model


rotateFrames : List ( Int, Int ) -> List ( Int, Int )
rotateFrames frames =
    case frames of
        [] ->
            []

        h :: tail ->
            tail ++ [ h ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate delta ->
            ( animate delta model |> move, Cmd.none )

        KeyOff code ->
            ( { model | pressed = Set.filter ((/=) code) model.pressed }
            , Cmd.none
            )

        KeyOn code ->
            ( keyOn code model, Cmd.none )

        LoadTexture (Err error) ->
            ( { model | texture = Error error }, Cmd.none )

        LoadTexture (Ok texture) ->
            ( { model | texture = Success texture }, Cmd.none )

        Resize size ->
            ( { model | window = size }, Cmd.none )

        VisibilityChange Hidden ->
            ( { model | state = Paused }, Cmd.none )

        VisibilityChange Visible ->
            ( { model | state = Playing 0 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs KeyOn
        , Keyboard.ups KeyOff
        , PageVisibility.visibilityChanges VisibilityChange
        , Window.resizes Resize
        ]



-- VIEW


currentFrame : List ( Int, Int ) -> ( Int, Int )
currentFrame frames =
    List.head frames |> Maybe.withDefault ( 0, 0 )


directionToFloat : Direction -> Float
directionToFloat direction =
    case direction of
        Left ->
            -1.0

        _ ->
            1.0


tupleToVec2 : ( Int, Int ) -> Vec2
tupleToVec2 tuple =
    let
        ( first, second ) =
            tuple
    in
        vec2 (toFloat first) (toFloat second)


view : Model -> Html Msg
view { direction, frames, texture, window } =
    WebGL.toHtml
        [ width window.width
        , height window.height
        , style [ ( "display", "block" ) ]
        ]
        (case texture of
            Success texture ->
                [ WebGL.entity
                    vertexShader
                    fragmentShader
                    mesh
                    { frameOffset = currentFrame frames |> tupleToVec2
                    , frameSize = vec2 80.0 110.0
                    , mirror = directionToFloat direction
                    , texture = texture
                    , textureSize = Texture.size texture |> tupleToVec2
                    , window = ( window.width, window.height ) |> tupleToVec2
                    }
                ]

            _ ->
                []
        )



-- MESH


type alias Vertex =
    { position : Vec2 }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec2 0 0)
          , Vertex (vec2 0 1)
          , Vertex (vec2 1 1)
          )
        , ( Vertex (vec2 0 0)
          , Vertex (vec2 1 0)
          , Vertex (vec2 1 1)
          )
        ]



-- SHADERS


type alias Uniforms =
    { frameOffset : Vec2
    , frameSize : Vec2
    , mirror : Float
    , texture : Texture
    , textureSize : Vec2
    , window : Vec2
    }


type alias Varyings =
    { texturePos : Vec2 }


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec2 position;
        uniform float mirror;
        uniform vec2 frameSize;
        uniform vec2 window;
        varying vec2 texturePos;

        void main () {
            vec2 centerBottom = vec2(
              (window.x / 2.0 - frameSize.x / 2.0),
              (window.y - frameSize.y)
            );

            vec2 pixelPos = position * frameSize + centerBottom;
            vec2 clipSpace = pixelPos / window * 2.0 - 1.0;

            gl_Position = vec4(clipSpace.x, -clipSpace.y, 0, 1);
            texturePos = vec2((position.x - 0.5) * mirror + 0.5, position.y);
        }

    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|

        precision mediump float;

        uniform sampler2D texture;
        uniform vec2 frameOffset;
        uniform vec2 frameSize;
        uniform vec2 textureSize;
        varying vec2 texturePos;

        void main () {
            vec2 size = frameSize / textureSize;
            vec2 space = size * (texturePos + frameOffset);
            vec4 sprite = texture2D(texture, space);

            gl_FragColor = vec4(sprite + vec4(0.0, 0.0, 0.0, 0.0));
        }
    |]
