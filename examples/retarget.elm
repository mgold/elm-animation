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
import Graphics.Element as E exposing (Element)
import Graphics.Collage as C exposing (Form)
import Time exposing (Time)
import Mouse
import Keyboard
import Window
import Debug

import Animation exposing (..)

mouseLocation : Signal (Float, Float)
mouseLocation =
  Signal.map2 (\(w,h) (x,y) -> (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y))
      Window.dimensions Mouse.position

type alias Pos = (Float, Float)
type alias Model =
    { clock : Time
    , x : Animation
    , y : Animation
    , trail : List Pos
    , clicks : List Pos
    , lastClickTime : Time
    , slow : Bool
    , smear : Bool
    }

dur = 750

model0 : Model
model0 = Model 0 (static 0) (static 0) [] [] 0 False False

type Action = Tick Time | Click Pos | Reset | NoOp | Slow Bool | Smear Bool

actions : Signal Action
actions =
    Signal.mergeMany
        [ Signal.map (\b -> if b then Reset else NoOp) Keyboard.enter
        , Signal.map Smear Keyboard.space
        , Signal.map Slow Keyboard.shift
        , Signal.map Click (Signal.sampleOn Mouse.clicks mouseLocation)
        , Signal.map Tick (Time.fps 50)
        ]

update : Action -> Model -> Model
update action model =
    case action of
        Tick dt -> updateTick dt model
        Click pos -> {model| clicks = pos::model.clicks --                 sync durations, very important
                           , x = retarget model.clock (fst pos) model.x |> duration dur
                           , y = retarget model.clock (snd pos) model.y |> duration dur
                           }
        Reset -> {model | clicks = List.head model.clicks |> Maybe.map (\c -> [c]) |> Maybe.withDefault []
                        , trail = []
                 }
        Slow b -> {model | slow = b}
        Smear b -> {model | smear = b}
        NoOp -> model


close a b = abs (a - b) < 1
updateTick : Time -> Model -> Model
updateTick dt model =
    let clock = model.clock + if model.slow then dt/5 else dt
        pos = (animate clock model.x, animate clock model.y)
        trail_dt = Time.millisecond * 30
        recentlyClicked = model.lastClickTime + trail_dt > clock
        lastClickTime = if recentlyClicked then model.lastClickTime else model.lastClickTime + trail_dt
        trail = case List.head model.trail of
            Nothing -> [pos]
            Just pos' -> if close (fst pos) (fst pos') && close (snd pos) (snd pos') || recentlyClicked
                         then model.trail
                         -- find the position for the time of the dot rather than using the current one
                         else (animate lastClickTime model.x, animate lastClickTime model.y) :: model.trail

    in {model| clock = clock, trail = trail, lastClickTime = lastClickTime}
        |> Debug.watchSummary "Bullet Time" .slow
        |> Debug.watchSummary "Smeared Time" .smear
        |> Debug.watchSummary "Animation ends in" (\{clock, x} -> (timeRemaining clock x)/1000)

model : Signal Model
model = Signal.foldp update model0 actions

acceleration : Time -> Animation -> Float
acceleration t a =
    let v0 = velocity (t-10) a
        v1 = velocity (t+10) a
    in (v1 - v0) / 20

render : (Int, Int) -> Model -> Element
render (w,h) model =
    C.collage w h <|
        renderClicks model ++
        renderTrail model ++
        [renderBall model]

renderBall model =
    let oneBall = renderBall' model.clock model.x model.y
    in if model.smear
    then List.map (\t -> renderBall' (model.clock + t*20) model.x model.y) [-5..5]
            |> C.group |> C.alpha 0.3 |> \gr -> C.group [oneBall, gr]
    else oneBall

renderBall' clock x y =
    let pos = (animate clock x, animate clock y)
        vel = (100*velocity clock x, 100*velocity clock y)
        acc = (10000*acceleration clock x, 10000*acceleration clock y)
    in C.group
        [ C.circle 20 |> C.filled Color.darkBlue
        , C.segment (0,0) vel |> thick Color.green
        , C.segment (0,0) acc |> thick Color.red
        ] |> C.move pos

renderTrail {trail} =
    List.map
        (\pos -> C.circle 2 |> C.filled Color.lightOrange |> C.move pos)
        trail

renderClicks {clicks} =
    List.indexedMap
        (\i pos -> C.square 12 |> C.filled (if i == 0 then Color.purple else Color.lightPurple) |> C.move pos)
        clicks

thick : Color -> C.Path -> Form
thick c =
    let style = C.solid c
    in C.traced {style| width = 2}

main = Signal.map2 render Window.dimensions model
