{- This example demonstrates the need to set the easing function of undone animations as `(\t -> 1 - (a.ease (1 - t)))`.
   Otherwise, undone animations will appear to work for symmetrical easing functions but not for asymmetric ones. The
   outer 1 - is necessary so that the from and to values may be set correctly, as they may be inspected.
-}
import Color
import Graphics.Collage as Collage
import Graphics.Element exposing (Element)
import Time exposing (Time)
import Window

import Animation exposing (..)

-- asymmetric ease in cubic
myEase x = x*x*x

type alias Model = { x : Animation, clock : Time, undone : Bool, dotsOut : List Float, dotsBack : List Float }
model0 = Model (animation 0 |> duration 4000 |> ease myEase) 0 False [] []

step : Time -> Model -> Model
step dt model =
    let clock = model.clock + dt
        pos = animate clock model.x
    in if not model.undone && pos > 0.999
       then {model | x = undo clock model.x, undone = True, dotsOut = pos :: model.dotsOut, dotsBack = [pos], clock = clock}
       else if not model.undone
       then {model | dotsOut = pos :: model.dotsOut, clock = clock}
       else {model | dotsBack = pos :: model.dotsBack, clock = clock}

model : Signal Model
model = Signal.foldp step model0 (Time.fps 20)

render : (Int, Int) -> Model -> Element
render (w, h) model =
    let circle = Collage.circle 8 |> Collage.filled Color.purple
                    |> Collage.move (toFloat w * animate model.clock model.x - toFloat (w//2), if model.undone then -30 else 0)
        dotsOut = List.map (\x -> Collage.circle 3 |> Collage.filled Color.lightPurple
                    |> Collage.moveX (toFloat w * x - toFloat (w//2))) model.dotsOut
        dotsBack = List.map (\x -> Collage.circle 3 |> Collage.filled Color.darkPurple
                    |> Collage.move (toFloat w * x - toFloat (w//2), -30)) model.dotsBack
    in Collage.collage w h <| circle :: dotsOut ++ dotsBack

main = Signal.map2 render Window.dimensions model
