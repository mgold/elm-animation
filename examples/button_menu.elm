{-| Button menu example. Implements the essential visual form implemented withn React Motion here:
https://medium.com/@nashvail/a-gentle-introduction-to-react-motion-dc50dd9f2459
Uses half as many lines, but the devil is in the details, and this isn't a usable component itself.
-}

import Color exposing (white, lightBlue, lightGray, darkGray)
import Graphics.Collage as Collage
import Graphics.Element exposing (Element)
import Mouse
import Time exposing (Time)
import Window

import Animation exposing (..)

mainButtonRad = 45
childButtonRad = 24
numChildren = 5

flyOutRad = 130
separationAngle = degrees 40
fanAngle = (numChildren - 1) * separationAngle
baseAngle = (pi - fanAngle)/2

type alias Pos = (Float, Float)
type Action = Tick Time | MouseClick Pos
type alias Model =
  { theta : Animation,
    rs : List Animation,
    clock : Time,
    open : Bool
  }

delayed i =
  delay (toFloat (numChildren-i) * 50)

model0 =
  let
    theta = animation -1000 |> from (degrees 45) |> to 0 |> duration 100
    rs = List.map (\i -> theta |> from flyOutRad |> delayed i) [0..numChildren-1]
  in
    Model theta rs 0 False

mouseLocation : Signal (Float, Float)
mouseLocation =
    Signal.map2 (\(w,h) (x,y) -> (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y))
        Window.dimensions Mouse.position

step : Action -> Model -> Model
step act model = case act of
    Tick dt ->
      { model | clock = model.clock + dt}

    MouseClick (x,y) ->
      if x^2 + y^2 < mainButtonRad^2 then
        { model |
          open = not model.open,
          theta = undo model.clock model.theta,
          rs = List.indexedMap (\i -> undo model.clock >> delayed i) model.rs
        }
      else model

actions : Signal Action
actions = Signal.mergeMany
    [ Signal.map Tick (Time.fps 60)
    , Signal.map MouseClick (Signal.sampleOn Mouse.clicks mouseLocation)
    ]

model : Signal Model
model = Signal.foldp step model0 actions

render : (Int, Int) -> Model -> Element
render (w, h) {rs, theta, clock} =
    let
      bg = Collage.rect (toFloat w) (toFloat h) |> Collage.filled lightGray

      angle = animate clock theta
      rect =  Collage.rect (mainButtonRad/5) (mainButtonRad/1.25) |> Collage.filled white
      circle = Collage.circle mainButtonRad |> Collage.filled lightBlue
      group = Collage.group [circle, rect, rect |> Collage.rotate (degrees 90)]
              |> Collage.rotate angle

      child func i a = Collage.circle childButtonRad |> func
                  |> Collage.move (fromPolar (animate clock a, baseAngle + separationAngle*toFloat i))
      stroked = Collage.outlined (Collage.solid darkGray)
      filled = Collage.filled white
      children = List.indexedMap (child stroked) rs ++ List.indexedMap (child filled) rs
    in
      Collage.collage w h (bg :: children ++ [group])

main = Signal.map2 render Window.dimensions model
