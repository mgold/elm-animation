module Pacman exposing (main)

{- This example shows a looping animation using only the undo method,
   not retarget. The step function is a little more
   repetitive than I’d like, suggesting a loop function.
   Without a major overhaul, the best implementation would be

       loop : Clock -> Animation -> Animation
       loop t a = if isDone t a then undo t a else a

   This requires the client to call the function on each invocation
   of update, which I consider unacceptable. Better to have this be
   handled under the covers, but that means expanding the `Animation`
   custom type. The best way to do that is probably define a
   `StandardAnimation` type and have all tags convert to it. Alternatively,
   come up with a sufficiently general representation and hope it isn’t
   too crazy to work with.

   Or stick it in a separate module — how often can you see multiple
   animations into the future? The trend seems to be the reverse direction,
   with physics simulations seeing only the next frame, handling interactions
   as they come rather than interrupting a plan. In the mean time,
   animations are certainly composable if the client does some of the work
   themselves.

   End brain dump.
-}

import Animation exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Json.Decode exposing (Value)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task exposing (Task)


type alias Model =
    { r : Animation
    , x : Animation
    , y : Animation
    , w : Int
    , h : Int
    , clock : Clock
    }


second : Float
second =
    1000


model0 =
    Model (animation 0 |> from 40 |> to 60 |> duration (0.2 * second))
        (animation 0 |> from 200 |> to -200 |> duration second)
        (animation 0 |> from 200 |> to -200 |> duration second |> delay second)
        0
        0
        0


type Msg
    = Tick Float
    | Resize Int Int
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            let
                clock =
                    model.clock + dt

                r =
                    if isDone clock model.r then
                        undo clock model.r

                    else
                        model.r

                moveDone =
                    isDone clock model.x && isDone clock model.y

                x =
                    if moveDone then
                        undo clock model.x

                    else
                        model.x

                y =
                    if moveDone then
                        undo clock model.y |> delay second

                    else
                        model.y
            in
            { model | clock = clock, r = r, x = x, y = y }

        Resize width height ->
            { model | w = width, h = height }

        NoOp ->
            model


view : Model -> Svg Msg
view { w, h, r, x, y, clock } =
    let
        radius =
            animate clock r

        posX =
            animate clock x + toFloat w / 2

        posY =
            animate clock y + toFloat h / 2
    in
    Svg.svg
        [ SA.style "position:absolute;left:0;top:0"
        , SA.width (String.fromInt w)
        , SA.height (String.fromInt h)
        ]
        [ Svg.circle
            [ SA.r (String.fromFloat radius)
            , SA.cx (String.fromFloat posX)
            , SA.cy (String.fromFloat posY)
            , SA.fill "yellow"
            ]
            []
        ]


subs : Sub Msg
subs =
    Sub.batch
        [ onResize Resize
        , onAnimationFrameDelta Tick
        ]


main : Program Value Model Msg
main =
    Browser.element
        { init =
            always
                ( model0
                , Task.perform
                    (\{ viewport } ->
                        Resize (round viewport.width) (round viewport.height)
                    )
                    getViewport
                )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = always subs
        , view = view
        }
