import Color
import Graphics.Element as E exposing (Element)
import Graphics.Collage as C exposing (Form)
import Time exposing (Time)
import Mouse
import Keyboard
import Window
import Debug

import Animation exposing (..)

{-| docs -}

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
        Click pos -> {model| clicks <- pos::model.clicks
                           , x <- retarget model.clock (fst pos) model.x |> duration dur
                           , y <- retarget model.clock (snd pos) model.y |> duration dur
                           }
        Reset -> {model | clicks <- List.head model.clicks |> Maybe.map (\c -> [c]) |> Maybe.withDefault []
                        , trail <- []
                 }
        Slow b -> {model | slow <- b}
        Smear b -> {model | smear <- b}
        NoOp -> model


close a b = abs (a - b) < 1
updateTick : Time -> Model -> Model
updateTick dt model =
    let dt' = if model.slow then dt/5 else dt
        clock = model.clock + dt'
        pos = (animate clock model.x, animate clock model.y)
        _ = model.lastClickTime
        recentlyClicked = model.lastClickTime + 10 > clock
        lastClickTime = if recentlyClicked then model.lastClickTime else clock
        trail = case List.head model.trail of
            Nothing -> [pos]
            Just pos' -> if close (fst pos) (fst pos') && close (snd pos) (snd pos') || recentlyClicked
                         then model.trail
                         else pos :: model.trail

    in {model| clock <- clock, trail <- trail, lastClickTime <- lastClickTime}
        |> Debug.watchSummary "Bullet Time" .slow
        |> Debug.watchSummary "Smeared Time" .smear
        |> Debug.watchSummary "Animation ends in" (\{clock, x} -> (timeRemaining clock x)/1000)

model : Signal Model
model = Signal.foldp update model0 actions

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
    in C.group
        [ C.circle 20 |> C.filled Color.darkBlue
        , C.segment (0,0) vel |> C.traced (C.solid Color.red)
        ] |> C.move pos

renderTrail {trail} =
    List.map
        (\pos -> C.circle 2 |> C.filled Color.lightPurple |> C.move pos)
        trail

renderClicks {clicks} =
    List.map
        (\pos -> C.square 12 |> C.filled Color.green |> C.move pos)
        clicks

main = Signal.map2 render Window.dimensions model
--main = Signal.map E.show model
