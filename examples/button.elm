import Color exposing (white, blue, darkBlue)
import Graphics.Collage as Collage
import Graphics.Element exposing (Element)
import Mouse
import Time exposing (Time)
import Window

import Animation exposing (..)

type alias Pos = (Float, Float)
type Action = Tick Time | MouseMove Pos | MouseClick Pos
-- Model = everything persisted between frames. State = state as in a state machine.
type State = Entering | Here | Growing | Big | Shrinking | Exiting | Gone
type alias Model = { r : Animation, theta : Animation, clock : Time, state : State}
model0 = Model (animation 0 |> from 0 |> to here_r) (static (degrees 45)) 0 Entering

dist (x,y) = sqrt <| x*x + y*y
collided pos {r, clock} = animate clock r > dist pos
here_r = 40
big_r = 70

mouseLocation : Signal (Float, Float)
mouseLocation =
    Signal.map2 (\(w,h) (x,y) -> (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y))
        Window.dimensions Mouse.position

-- this case analysis is ugly but I think it's intrinsic to the component
step : Action -> Model -> Model
step act model = case act of
    Tick t -> let clock = model.clock + t
                  radiusDone = isDone clock model.r
                  state = case model.state of
                    Entering -> if radiusDone then Here else Entering
                    Growing -> if radiusDone then Big else Growing
                    Shrinking -> if radiusDone then Here else Shrinking
                    Exiting -> if radiusDone then Gone else Exiting
                    Gone -> if isRunning clock model.r then Entering else Gone
                    _ -> model.state
              in if model.state == Gone && radiusDone
                 then {model| clock = clock, r = (animation clock |> from 0 |> to here_r |> delay 1500)}
                 else {model| clock = clock, state = state}

    MouseMove pos -> let now = model.clock
                         collision = collided pos model
                         growingOrBig = model.state == Growing || model.state == Big
                     in if not collision && growingOrBig
                        then {model| r = undo now model.r
                                   , theta = undo now model.theta
                                   , state = Shrinking}
                        else if collision && model.state == Here
                        then {model| r = retarget now big_r model.r |> duration 150
                                   , theta = retarget now (degrees -45) model.theta |> duration 200
                                   , state = Growing}
                        else model

    MouseClick pos -> if collided pos model
                      then {model| r = retarget model.clock 0 model.r |> duration 750,
                                   theta = retarget model.clock (degrees 45) model.theta |> duration 750,
                                   state = Exiting }
                      else model

actions : Signal Action
actions = Signal.mergeMany
    [ Signal.map Tick (Time.fps 60)
    , Signal.map MouseMove mouseLocation
    , Signal.map MouseClick (Signal.sampleOn Mouse.clicks mouseLocation)
    ]

model : Signal Model
model = Signal.foldp step model0 actions

render : (Int, Int) -> Model -> Element
render (w, h) {r, theta, clock} =
    let radius = animate clock r
        angle = animate clock theta
        rect =  Collage.rect (radius/8) (radius/1.25) |> Collage.filled white
        circle = Collage.circle radius |> Collage.filled darkBlue
        group = Collage.group [circle, rect, rect |> Collage.rotate (degrees 90)]
                |> Collage.rotate angle
    in Collage.collage w h [group]

main = Signal.map2 render Window.dimensions model
