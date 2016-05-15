{- This example shows animating to an angle. The only slightly tricky bit is making sure you go the shortest way around.
-}
import Color
import Graphics.Collage as Collage
import Graphics.Element exposing (Element)
import Time exposing (Time)
import Window
import Mouse

import Animation exposing (..)

type alias Model = { theta : Animation, r : Animation, clock : Time }
model0 = Model (static 0) (static 100) 0

mouseLocation : Signal (Float, Float)
mouseLocation =
    Signal.map2 (\(w,h) (x,y) -> toPolar (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y))
        Window.dimensions Mouse.position

type Action = Tick Time | Click (Float, Float)

actions : Signal Action
actions = Signal.merge
    (Signal.map Click (Signal.sampleOn Mouse.clicks mouseLocation))
    (Signal.map Tick (Time.fps 60))

step : Action -> Model -> Model
step act model = case act of
    Tick dt -> {model| clock = model.clock + dt}
    Click (dest_r, dest_theta) ->
        let theta = retarget model.clock dest_theta model.theta |> speed 0.002 |> normalizeAngle
            r = retarget model.clock (min armLength dest_r) model.r |> speed 0.4
            dur = max (getDuration theta) (getDuration r)
        in {model| theta = theta |> duration dur, r = r |> duration dur}

normalizeAngle anim =
    let from = getFrom anim
        to = getTo anim
    in if abs (from - to) < (degrees 180) then anim
       else if to < from then normalizeAngle <| Animation.to (to + turns 1) anim
       else normalizeAngle <| Animation.to (to - turns 1) anim

model : Signal Model
model = Signal.foldp step model0 actions

armLength = 400
render : (Int, Int) -> Model -> Element
render (w, h) {theta, r, clock} =
    let angle = animate clock theta
        radius = animate clock r
        base = Collage.circle 5 |> Collage.filled Color.charcoal
        arm = Collage.rect armLength 3 |> Collage.filled Color.charcoal |> Collage.moveX (armLength / 2)
        circle = Collage.circle 20 |> Collage.filled Color.red |> Collage.moveX radius
        group = Collage.group [base, arm, circle] |> Collage.rotate angle
    in Collage.collage w h [group]

main = Signal.map2 render Window.dimensions model
