# Animation for Elm
A library for animating between two Float values. For example, animate a panel's width from 100px to 300px over 2
seconds, or make a button spin and grow on hover. Everything is a pure function, no signals in sight, so you can use it
easily within your architecture. You can also inspect animations to determine if they are still running and for how
long, and even smoothly retarget a different destination midflight.

The library encapsulates a 3-stage animation pipeline:

* **Timekeeping:** You are expected to maintain a running clock, which could be as simple as `Signal.foldp (+) 0 (Time.fps 30)`
    or part of your state and action types. You use this clock to create an animation and again to obtain the current
    value. You can also specify a delay and the duration for your animation.

* **Easing:** An easing function is what makes an animation come alive with acceleration and sometimes even elasticity.
    When setting the easing function for your animation, I recommend [Dan's
    library](http://package.elm-lang.org/packages/Dandandan/Easing/latest/Easing#easing-functions). (Dan also has your
    back if you need to interpolate pairs or colors with the output of this library.)

* **Interpolation:** It wouldn't be very useful is all animations went from 0 to 1 (the default), would it? You can
    specify values to animate `from` and `to`. Furthermore, you can set the average speed (distance between these two
    values per milisecond) instead of a duration.

Once you have your value at the current time, you can render it to any frontend you choose: Collage, Element, Html,
[turtles](http://package.elm-lang.org/packages/mgold/elm-turtle-graphics/latest)...

## Basic Usage

````elm
import Animation exposing (..)
import Time exposing (second)

myAnim = animation 0 |> from 100 |> to 300 |> duration (4*second) |> delay (1*second)
List.map (\t -> animate (t*second) myAnim) [0..6]
-- [100, 100, 129.29, 200, 270.71, 300, 300]
````

Notice that the value remains constant during the delay and after the animation is complete, and that easing in and out
is applied by default.
