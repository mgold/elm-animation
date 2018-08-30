# elm-animation

Animate `Float` values with control over their start and end values, duration, easing, and so on. For example, animate a panel's width from 100px to 300px over 2 seconds, or make a button spin and grow on hover. Supports advanced features like smoothly retargeting to a new destination while in flight.

This library is fairly low-level from the perspective of building web apps. Many users will be better served by [mdgriffith/elm-style-animation](https://package.elm-lang.org/packages/mdgriffith/elm-style-animation/latest/).

See more end-to-end example code in the `examples/` folder.

## Design Goals

The library encapsulates a 3-stage animation pipeline:

* **Timekeeping:** Creating and running an animation requires the current time, which is best obtained by summing
    [`Browser.Events.onAnimationFrame`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onAnimationFrame).
    You can also specify the duration of animation, and delay it prior to starting.

* **Easing:** An easing function makes an animation come alive with acceleration or even elasticity. You can find all
    kinds of crazy easing functions in [this library](http://package.elm-lang.org/packages/elm-community/easing-functions/latest/Ease).

* **Interpolation:** It wouldn't be very useful is all animations went from 0 to 1 (the default), would it? You can
    specify values to animate `from` and `to`. Furthermore, you can set the average speed (distance between these two
    values per milisecond) instead of a duration.

Because the output of this library is a `Float`, you can use it with `Html`, `Svg`, or any other frontend.

## Overview

`animation` creates an animation starting at the given time (usually the current time). `animate` takes the current time
and an animation, and produces the current value. Animations go through three phases: they are scheduled, they run, and then they are done.

````elm
import Animation exposing (..)
import Time

myAnim : Animation
myAnim = animation 0
  |> from 100
  |> to 300
  |> duration 4
  |> delay 1

List.map (\t -> animate t myAnim) [0, 1, 2, 3, 4, 5, 6]
--> [100, 100, 129.28932188134524, 200, 270.71067811865476, 300, 300]
````

Notice that the value remains constant during the delay and after the animation is done. You can also use `static` to
create animations of constant value. By using these two degenerate cases, you ought to be able to keep animations in
your model without worrying about when they aren't actually animating.
