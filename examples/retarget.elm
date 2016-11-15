module Main exposing (..)

{-| A blue ball moves towards wherever you click.
  Shown onscreen:
    * Blue ball: current position of object
    * Green line: velocity vector
    * Red line: acceleration vector
    * Orange dots: location sampled at regular intervals
    * Purple squares: destinations (current one is darker)
  Controls:
    * Click to set destination (including while ball is in motion)
    * Shift to slow down time
    * Space to see the interpolated past and future
    * Enter to clear the history
  Notice:
    * When not retargeted, the velocity and acceleration vectors are in the line of travel
    * When retargeted, acceleration changes instantly but velocity does not
    * When retargeted, the interpolated past and future change but the present does not.
    * Each trip takes a constant duration, meaning shorter trips are slower. One could instead specify speed.
-}

import Color exposing (Color)
import Element as E exposing (Element)
import Collage as C exposing (Form)
import Time exposing (Time)
import Task
import Mouse
import Keyboard
import Window
import AnimationFrame
import Html exposing (program)
import Animation exposing (..)


{-
   mouseLocation : Signal ( Float, Float )
   mouseLocation =
     Signal.map2
       (\( w, h ) ( x, y ) -> ( toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y ))
       Window.dimensions
       Mouse.position
-}


type alias Pos =
    { x : Float, y : Float }


type alias Model =
    { clock : Time
    , x : Animation
    , y : Animation
    , w : Int
    , h : Int
    , trail : List Pos
    , clicks : List Pos
    , lastClickTime : Time
    , slow : Bool
    , smear : Bool
    }


dur =
    750


model0 : Model
model0 =
    Model 0 (static 0) (static 0) 0 0 [] [] 0 False False


type Msg
    = Tick Time
    | Resize Window.Size
    | Click Mouse.Position
    | Reset
    | NoOp
    | Slow Bool
    | Smear Bool


keycode =
    { enter = 13, shift = 16, space = 32 }


subs : Sub Msg
subs =
    Sub.batch
        [ Keyboard.downs
            (\code ->
                if code == keycode.enter then
                    Reset
                else if code == keycode.shift then
                    Slow True
                else if code == keycode.space then
                    Smear True
                else
                    Reset
            )
        , Keyboard.ups
            (\code ->
                if code == keycode.shift then
                    Slow False
                else if code == keycode.space then
                    Smear False
                else
                    Reset
            )
        , Mouse.clicks Click
        , Window.resizes Resize
        , AnimationFrame.diffs Tick
        ]


update : Msg -> Model -> Model
update action model =
    case action of
        Tick dt ->
            updateTick dt model

        Resize { width, height } ->
            { model | w = width, h = height }

        Click rawPos ->
            let
                pos =
                    { x = toFloat rawPos.x - toFloat model.w / 2, y = toFloat model.h / 2 - toFloat rawPos.y }
            in
                { model
                    | clicks =
                        pos :: model.clicks
                        --                 sync durations, very important
                    , x = retarget model.clock pos.x model.x |> duration dur
                    , y = retarget model.clock pos.y model.y |> duration dur
                }

        Reset ->
            { model
                | clicks = List.head model.clicks |> Maybe.map (\c -> [ c ]) |> Maybe.withDefault []
                , trail = []
            }

        Slow b ->
            { model | slow = b }

        Smear b ->
            { model | smear = b }

        NoOp ->
            model


close a b =
    abs (a - b) < 1


updateTick : Time -> Model -> Model
updateTick dt model =
    let
        clock =
            model.clock
                + if model.slow then
                    dt / 5
                  else
                    dt

        pos =
            { x = animate clock model.x, y = animate clock model.y }

        trail_dt =
            Time.millisecond * 30

        recentlyClicked =
            model.lastClickTime + trail_dt > clock

        lastClickTime =
            if recentlyClicked then
                model.lastClickTime
            else
                model.lastClickTime + trail_dt

        trail =
            case List.head model.trail of
                Nothing ->
                    [ pos ]

                Just pos_ ->
                    if close pos.x pos_.x && close pos.y pos_.y || recentlyClicked then
                        model.trail
                        -- find the position for the time of the dot rather than using the current one
                    else
                        { x = animate lastClickTime model.x, y = animate lastClickTime model.y } :: model.trail
    in
        { model | clock = clock, trail = trail, lastClickTime = lastClickTime }


acceleration : Time -> Animation -> Float
acceleration t a =
    let
        v0 =
            velocity (t - 10) a

        v1 =
            velocity (t + 10) a
    in
        (v1 - v0) / 20


render : Model -> Element
render model =
    C.collage model.w model.h <|
        renderClicks model
            ++ renderTrail model
            ++ [ renderBall model ]


renderBall model =
    let
        oneBall =
            renderBall_ model.clock model.x model.y
    in
        if model.smear then
            List.map (\t -> renderBall_ (model.clock + toFloat t * 20) model.x model.y) (List.range -5 5)
                |> C.group
                |> C.alpha 0.3
                |> \gr -> C.group [ oneBall, gr ]
        else
            oneBall


renderBall_ clock x y =
    let
        pos =
            ( animate clock x, animate clock y )

        vel =
            ( 100 * velocity clock x, 100 * velocity clock y )

        acc =
            ( 10000 * acceleration clock x, 10000 * acceleration clock y )
    in
        C.group
            [ C.circle 20 |> C.filled Color.darkBlue
            , C.segment ( 0, 0 ) vel |> thick Color.green
            , C.segment ( 0, 0 ) acc |> thick Color.red
            ]
            |> C.move pos


renderTrail { trail } =
    List.map
        (\{ x, y } -> C.circle 2 |> C.filled Color.lightOrange |> C.move ( x, y ))
        trail


renderClicks { clicks } =
    List.indexedMap
        (\i { x, y } ->
            C.square 12
                |> C.filled
                    (if i == 0 then
                        Color.purple
                     else
                        Color.lightPurple
                    )
                |> C.move ( x, y )
        )
        clicks


thick : Color -> C.Path -> Form
thick c =
    let
        style =
            C.solid c
    in
        C.traced { style | width = 2 }


main =
    program
        { init = ( model0, Task.perform Resize Window.size )
        , update = (\msg model -> ( update msg model, Cmd.none ))
        , subscriptions = always subs
        , view = render >> E.toHtml
        }
