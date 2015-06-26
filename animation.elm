module Animation where -- TODO: export only public things

{-| A library for animating between two Float values. For example, animate a panel's width from 100px to 300px over 2
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

# Basic Usage

````elm
import Animation exposing (..)
import Time exposing (second)

myAnim = animation 0 |> from 100 |> to 300 |> duration (4*second) |> delay (1*second)
List.map (\t -> animate (t*second) myAnim) [0..6]
-- [100, 100, 129.29, 200, 270.71, 300, 300]
````
Notice that the value remains constant during the delay and after the animation is complete, and that easing in and out
is applied by default.

# Create
@docs animation, static

# Run
@docs animate

# Modify
## Settings
You may set an animation's duration or speed but not both, since one determines the other.
@docs duration, speed, delay, ease, from, to

## Interruptions
@docs undo, retarget

# Inspect
## Settings
getDuration, getSpeed, getDelay, getEase, getFrom, getTo

## Physics
@docs velocity, timeRemaining

## Lifecycle
@docs isScheduled, isRunning, isDone

-}

import Time exposing (Time)
import Easing as Dan

-- private
type DurationOrSpeed = Duration Time | Speed Float

-- private - TODO make an opaque union type
type alias Animation = { start : Time , delay : Time , dos : DurationOrSpeed
                       , ramp : Maybe Float -- used for interruptions
                       , ease : Float -> Float
                       , from : Float , to : Float
                       }

-- private
dur : DurationOrSpeed -> Float -> Float -> Time
dur dos from to =
    case dos of
        Duration t -> t
        Speed s -> (abs (to - from)) / s

-- private
spd : DurationOrSpeed -> Float -> Float -> Float
spd dos from to =
    case dos of
        Duration t -> (abs (to - from)) / t
        Speed s -> s

{-| Create an animation that begins now. By default, animations have no delay, last 750ms, and interpolate between 0 and
1 with a sinusoidal easing function. All of these can be changed.
-}
animation : Time -> Animation
animation now = Animation now 0 (Duration 750) Nothing Dan.easeInOutSine 0 1

{-| Create a static animation that is always the given value.
-}
static : Float -> Animation
static x = Animation 0 0 (Duration 0) Nothing identity x x
-- TODO: passing this to other functions will be bad. If we need to use a union type, make this a tag?

{-| Produce the value of an animation at a given time.
-}
animate : Time -> Animation -> Float
animate t {start, delay, dos, ramp, from, to, ease}  =
    let duration = dur dos from to
        fr = clamp 0 1 <| (t - start - delay) / duration
        eased = ease fr
        correction = case ramp of
            Nothing -> 0
            Just vel -> let eased' = Dan.easeInOutSine fr -- always use cosine ease for this
                            from' = vel * (t - start)
                        in from' - from'*eased'
    in from + (to-from)*eased + correction

{-| Run an animation in reverse from its current state, beginning immediately.
-}
undo : Time -> Animation -> Animation
undo t a =
    let remaining = timeRemaining t a
    in {a| from <- a.to, to <- a.from, start <- t-remaining, delay <- 0}

{-| Change the `to` value of a running animation, without an abrupt acceleration or jerk. The easing function will be
retained (but you can change it with `ease`). A new speed and duration will be chosen based on what makes the animation
smooth. It is safe to retarget animations that are scheduled and done.
-}
retarget : Time -> Float -> Animation -> Animation
retarget t newTo a =
    if | isScheduled t a -> {a| to <- newTo}
       | isDone t a -> {a| start <- t, from <- a.to, to <- newTo}
       | otherwise ->
            let vel = velocity t a
                pos = animate t a
            in Animation t 0 (Speed (vel/3)) (Just vel) a.ease pos newTo

{-| Set the duration of an animation. This setting overrides, and is overriden by, `speed` (last application wins).
-}
duration : Time -> Animation -> Animation
duration x r = {r| dos <- Duration x}

{-| Set the _average_ speed of an animation. Speed is the rate at which the animation progresses between the `from` and
`to` values per milisecond. Most easing functions will deviate from the average speed. You do not need to worry about
the sign. It is safe to alter the `from` and `to` values after setting speed, but a set speed will be overwritten by
`duration`.
-}
speed : Float -> Animation -> Animation
speed x r = {r| dos <- Speed (abs x)}

{-| Set the delay of an animation. An animation will not start until after the delay. The default delay is 0.
-}
delay : Time -> Animation -> Animation
delay x r = {r| delay <- x}

{-| Set the easing function of an animation. It is expected that `f 0 == 0` and `f 1 == `. The default is a sinusoidal
in-out.
-}
ease : (Float -> Float) -> Animation -> Animation
ease x r = {r| ease <- x}

{-| Set the initial value of an animation. The default is 0.
-}
from : Float -> Animation -> Animation
from x r = {r| from <- x}

{-| Set the final value of an animation. The default is 1.
-}
to : Float -> Animation -> Animation
to x r = {r| to <- x}

{-| Get the total duration of an animation, not counting delay.
-}
getDuration : Animation -> Time
getDuration {dos, from, to} = dur dos from to

{-| Get the time that the animation has yet to play before becoming done. Will be zero for animations that are already
done.
-}
timeRemaining : Time -> Animation -> Time
timeRemaining t {start, delay, dos, from, to} =
    let duration = dur dos from to
    in start+delay+duration - t |> max 0

{-| Get the _current_ velocity of the animation, aproximated by looking 10ms forwards and backwards (the central
difference). The velocity may be negative.
-}
velocity : Time -> Animation -> Float
velocity t a =
    let backDiff = animate (t-10) a
        forwDiff = animate (t+10) a
    in (forwDiff - backDiff) / 20

-- TODO: other getters
getTo a = a.to
setStart t a = {a| start <- t}
-- for docs?
--Speed is the rate at which the animation progresses between the `from` and `to` values per milisecond.
--You can get the speed even if you specified duration instead.


{-| Determine if an animation is scheduled, meaning that it has not yet changed value.
-}
isScheduled : Time -> Animation -> Bool
isScheduled t {start, delay} =
    t <= start+delay

{-| Determine if an animation is running, meaning that it is currently changing value.
-}
isRunning : Time -> Animation -> Bool
isRunning t {start, delay, dos, from, to} =
    let duration = dur dos from to
    in t > start+delay && t < start+delay+duration

{-| Determine if an animation is done, meaning that it has arrived at its final value.
-}
isDone : Time -> Animation -> Bool
isDone t {start, delay, dos, from, to} =
    let duration = dur dos from to
    in t >= start+delay+duration
