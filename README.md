# Animation for Elm
A library for animating between two `Float` values. For example, animate a panel's width from 100px to 300px over 2
seconds, or make a button spin and grow on hover. Everything is a pure function (no signals or tasks), so you can use it
easily within your architecture. You can also inspect animations to determine if they are still running and for how
long, and even smoothly retarget a different destination midflight.

The library encapsulates a 3-stage animation pipeline:

* **Timekeeping:** You are expected to maintain a running clock. If you are using `elm-effects`, request `Effects.tick`
    with an action to store the clock in the model. Otherwise, use `Time.fps` or `AnimationFrame.frame`, which provide
    time *deltas* that you will have to sum either in your model or with using `Signal.foldp (+) 0`. However obtained,
    this clock is used to create an animation and again to obtain the current value. You can also specify a delay and
    the duration for your animation.

* **Easing:** An easing function is what makes an animation come alive with acceleration and sometimes even elasticity.
    When setting the easing function for your animation, I recommend [Dan's
    library](http://package.elm-lang.org/packages/Dandandan/Easing/latest/Easing#easing-functions). (Dan also has your
    back if you need to interpolate pairs or colors with the output of this library.)

* **Interpolation:** It wouldn't be very useful is all animations went from 0 to 1 (the default), would it? You can
    specify values to animate `from` and `to`. Furthermore, you can set the average speed (distance between these two
    values per milisecond) instead of a duration.

Once you have your value at the current time, you can render it to any frontend you choose: Collage, Element, Html,
[Turtles](http://package.elm-lang.org/packages/mgold/elm-turtle-graphics/latest)...

## Basic Usage
`animation` creates an animation starting at the given time (usually the current time). `animate` takes the current time
and an animation, and produces the current value. Animations go through three phases (not related to the three stages of
rendering): they are scheduled, they run, and then they are done.

````elm
import Animation exposing (..)
import Time exposing (second)

myAnim = animation 0 |> from 100 |> to 300 |> duration (4*second) |> delay (1*second)
List.map (\t -> animate (t*second) myAnim) [0..6]
-- [100, 100, 129.29, 200, 270.71, 300, 300]
````

Notice that the value remains constant during the delay and after the animation is done. You can also use `static` to
create animations of constant value. By using these two degenerate cases, you ought to be able to keep animations in
your model without worrying about when they aren't actually animating.
