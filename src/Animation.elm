module Animation
    exposing
        ( Animation
        , Clock
        , TimeDelta
        , animate
        , animation
        , delay
        , duration
        , ease
        , equals
        , from
        , getDelay
        , getDuration
        , getEase
        , getFrom
        , getSpeed
        , getStart
        , getTo
        , getVelocity
        , isDone
        , isRunning
        , isScheduled
        , retarget
        , speed
        , static
        , timeElapsed
        , timeRemaining
        , to
        , undo
        )

{-| Animate between two `Float` values. For examples and general usage, see the README.


# Types

Animators have to keep track of different sorts of numbers. All of these are `Float`s under the hood, so they exist to
clarify documentation. Be warned that the compiler won't help you keep them straight.

  - `Clock` refers to a point in time, identified by duration since an epoch. For our purposes, the epoch is typically page load. (Why not use the UNIX epoch? Sometimes we have to take the sine of the clock and that's expensive for large numbers.) You are responsible for maintaining the clock and passing it in as necessary, most notably to run and create animations.

  - `TimeDelta` refers to the difference between two `Clock` times, such as a duration or a delay.

  - `Value` refers to the value being animated. It's used to indicate the output of the animation, as well as the initial
    `from` and final `to` values.

  - A plain `Float` refers to the speed at which the `Value` changes.

@docs TimeDelta, Clock, Animation


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


## Equality

@docs equals


## Lifecycle

@docs isScheduled, isRunning, isDone


## Physics

@docs timeElapsed, timeRemaining, getVelocity


## Settings

@docs getStart, getDuration, getSpeed, getDelay, getEase, getFrom, getTo

-}

import Time exposing (Posix)


{-| A type alias for amount of time (milliseconds) passed since page load.
-}
type alias Clock =
    Float


{-| A type alias for the difference between two `Clock` times.
-}
type alias TimeDelta =
    Float


{-| A type alias for the value being animated.
-}
type alias Value =
    Float


{-| private
-}
type DurationOrSpeed
    = Duration TimeDelta
    | Speed Float


{-| private
-}
type alias AnimRecord =
    { start : Clock
    , delay_ : TimeDelta
    , dos : DurationOrSpeed
    , ramp : Maybe Float -- used for interruptions
    , ease_ : Float -> Float
    , from_ : Value
    , to_ : Value
    }


{-| An Animation is an opaque type that represents a time-varying number (floating point value).
-}
type Animation
    = A AnimRecord


{-| private
-}
dur : DurationOrSpeed -> Value -> Value -> TimeDelta
dur dos from_ to_ =
    case dos of
        Duration t ->
            t

        Speed s ->
            abs (to_ - from_) / s


{-| private
-}
spd : DurationOrSpeed -> Value -> Value -> Float
spd dos from_ to_ =
    case dos of
        Duration t ->
            abs (to_ - from_) / t

        Speed s ->
            s



--private


defaultDuration : DurationOrSpeed
defaultDuration =
    Duration 750



--private


defaultEase : Float -> Float
defaultEase x =
    (1 - cos (pi * x)) / 2


{-| Create an animation that begins at the given time. By default, animations have no delay, last 750ms, and interpolate
between 0 and 1 with a sinusoidal easing function. All of these can be changed.
-}
animation : Clock -> Animation
animation t =
    A <| AnimRecord t 0 defaultDuration Nothing defaultEase 0 1


{-| Create a static animation that is always the given value.
-}
static : Value -> Animation
static x =
    A <| AnimRecord 0 0 defaultDuration Nothing defaultEase x x


{-| Produce the value of an animation at a given time.
-}
animate : Clock -> Animation -> Value
animate clock (A { start, delay_, dos, ramp, from_, to_, ease_ }) =
    let
        duration_ =
            dur dos from_ to_

        fr =
            clamp 0 1 <| (clock - start - delay_) / duration_

        eased =
            ease_ fr

        correction =
            case ramp of
                Nothing ->
                    0

                Just vel ->
                    let
                        -- always use cosine ease for this
                        eased_ =
                            defaultEase fr

                        from__ =
                            vel * (clock - start)
                    in
                    from__ - from__ * eased_

        -- TODO do we properly interpolate when the easing function isn't a sinusoid?
    in
    from_ + (to_ - from_) * eased + correction


{-| Run an animation in reverse from its current state, beginning immediately (even if the animation was delayed or has
been done for a while).

Usually you don't want to undo an animation that has been retargeted; just retarget it again. Similarly, undoing an
undone animation is frequently not what you want.

-}
undo : Clock -> Animation -> Animation
undo clock ((A a) as u) =
    A
        { a
            | from_ = a.to_
            , to_ = a.from_
            , start = clock
            , delay_ = -(timeRemaining clock u)
            , ramp = Nothing
            , ease_ = \clock2 -> 1 - a.ease_ (1 - clock2)
        }


{-| Change the `to` value of a running animation, without an abrupt change in velocity. The easing function will be
retained (but you can change it with `ease`). The animation will retain its average speed (but not necessarily
duration). If you retarget multiple animations at once (e.g. x and y), you will need to sync their durations (perhaps to
the `timeRemaining` in the old animations).

If the retargeted animation is still scheduled, the `to` value is replaced. If it's already done, `from` becomes the
old `to`, `to` and `start` are set to the values provided, and the delay is set to zero. If the old and new `to` values
are the same, the animation is unchanged.

-}
retarget : Clock -> Value -> Animation -> Animation
retarget clock newTo ((A a) as u) =
    if newTo == a.to_ then
        u

    else if isStatic u then
        A { a | start = clock, to_ = newTo, ramp = Nothing }

    else if isScheduled clock u then
        A { a | to_ = newTo, ramp = Nothing }

    else if isDone clock u then
        A { a | start = clock, delay_ = 0, from_ = a.to_, to_ = newTo, ramp = Nothing }

    else
        -- it's running
        let
            vel =
                getVelocity clock u

            pos =
                animate clock u

            newSpeed =
                case a.dos of
                    -- avoid recreating this object
                    Speed _ ->
                        a.dos

                    Duration _ ->
                        Speed (spd a.dos a.from_ a.to_)
        in
        A <| AnimRecord clock 0 newSpeed (Just vel) a.ease_ pos newTo


{-| Set the duration of an animation to the time specified.
-}
duration : TimeDelta -> Animation -> Animation
duration x (A a) =
    A { a | dos = Duration x }


{-| Set the _average_ speed of an animation. Speed is the rate at which the animation progresses between the `from` and
`to` values per milisecond. Most easing functions will deviate from the average speed. You do not need to worry about
the sign. It is safe to alter the `from` and `to` values after setting speed.
-}
speed : Float -> Animation -> Animation
speed x (A a) =
    A { a | dos = Speed (abs x) }


{-| Set the delay of an animation to the time specified. An animation will not start until after the delay. The default
delay is 0.
-}
delay : TimeDelta -> Animation -> Animation
delay x (A a) =
    A { a | delay_ = x }


{-| Set the easing function of an animation. It is expected that `f 0 == 0` and `f 1 == 1`. The default is a sinusoidal
in-out.
-}
ease : (Float -> Float) -> Animation -> Animation
ease x (A a) =
    A { a | ease_ = x }


{-| Set the initial value of an animation. The default is 0.
-}
from : Value -> Animation -> Animation
from x (A a) =
    A { a | from_ = x, ramp = Nothing }


{-| Set the final value of an animation. The default is 1.

For animations that are already running, use `retarget`.

-}
to : Value -> Animation -> Animation
to x (A a) =
    A { a | to_ = x, ramp = Nothing }


{-| Get the time elapsed since the animation started playing (after the end of delay). Will be zero for animations that
are still scheduled, and is not bounded for animations that are already done.
-}
timeElapsed : Clock -> Animation -> TimeDelta
timeElapsed clock ((A { start, delay_ }) as u) =
    if isStatic u then
        0

    else
        clock - (start + delay_) |> max 0


{-| Get the time until the animation is done. This time may be spent animating or be part of the delay. Will be zero for animations
that are already done.
-}
timeRemaining : Clock -> Animation -> TimeDelta
timeRemaining clock ((A { start, delay_, dos, from_, to_ }) as u) =
    if isStatic u then
        0

    else
        let
            duration_ =
                dur dos from_ to_
        in
        start + delay_ + duration_ - clock |> max 0


{-| Get the _current_ velocity of the animation, aproximated by looking 10ms forwards and backwards (the central
difference). The velocity may be negative.
-}
getVelocity : Clock -> Animation -> Float
getVelocity clock u =
    let
        backDiff =
            animate (clock - 10) u

        forwDiff =
            animate (clock + 10) u
    in
    (forwDiff - backDiff) / 20


{-| Get the start time of the animation, not accounting for delay. For animations created with `animate`, this is the
argument that was passed. For interrupted animations, this is when the interruption occured.
-}
getStart : Animation -> Clock
getStart (A a) =
    a.start


{-| Get the duration of the animation, not counting delay.
-}
getDuration : Animation -> TimeDelta
getDuration (A { dos, from_, to_ }) =
    dur dos from_ to_


{-| Get the average speed of the animation.
-}
getSpeed : Animation -> Float
getSpeed (A { dos, from_, to_ }) =
    spd dos from_ to_


{-| Get the delay of the animation.
-}
getDelay : Animation -> TimeDelta
getDelay (A a) =
    a.delay_


{-| Get the easing function of the animation.
-}
getEase : Animation -> Float -> Float
getEase (A a) =
    a.ease_


{-| Get the initial value of the animation.
-}
getFrom : Animation -> Value
getFrom (A a) =
    a.from_


{-| Get the final value of the animation.
-}
getTo : Animation -> Value
getTo (A a) =
    a.to_


{-| Equality on animations. Compared to `(==)` (which should not be used), this
function handles the conversion of speed and duration, and start and delay. It
also samples the easing functions, which may produce false positives (but
usually not in practice).

    equals (animation 0) (animation 0) --> True
    equals (animation 0 |> delay 10) (animation 10) --> True
    equals (animation 0 |> duration 1000) (animation 0 |> speed 0.001) --> True

    equals (static 0) (animation 0) --> False
    equals (animation 0 |> from -1) (animation 0) --> False
    equals (animation 0 |> ease identity) (animation 0) --> False

-}
equals : Animation -> Animation -> Bool
equals (A a) (A b) =
    (a.start + a.delay_ == b.start + b.delay_)
        && (a.from_ == b.from_)
        && (a.to_ == b.to_)
        && (a.ramp == b.ramp)
        && (a.dos == b.dos || 0.001 >= abs (dur a.dos a.from_ a.to_ - dur b.dos b.from_ b.to_))
        && List.all (\t -> a.ease_ t == b.ease_ t) [ 0.1, 0.3, 0.7, 0.9 ]


{-| private , currently. Any value in exporting?
-}
isStatic : Animation -> Bool
isStatic (A { from_, to_ }) =
    from_ == to_


{-| Determine if an animation is scheduled, meaning that it has not yet changed value.
-}
isScheduled : Clock -> Animation -> Bool
isScheduled clock ((A { start, delay_ }) as u) =
    clock <= start + delay_ && not (isStatic u)


{-| Determine if an animation is running, meaning that it is currently changing value.

Static animations are never running.

-}
isRunning : Clock -> Animation -> Bool
isRunning clock ((A { start, delay_, dos, from_, to_ }) as u) =
    let
        duration_ =
            dur dos from_ to_
    in
    clock > start + delay_ && clock < start + delay_ + duration_ && not (isStatic u)


{-| Determine if an animation is done, meaning that it has arrived at its final value.

Static animations are always done.

-}
isDone : Clock -> Animation -> Bool
isDone clock ((A { start, delay_, dos, from_, to_ }) as u) =
    let
        duration_ =
            dur dos from_ to_
    in
    isStatic u || clock >= start + delay_ + duration_
