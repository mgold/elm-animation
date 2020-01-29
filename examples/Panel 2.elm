module Panel exposing (main)

{- In this example, rather than steadily counting time and replacing
   animations, we rock time back and forth and maintain the same animations.
   A naive implementation, shown as the top panel, reverses direction
   immediately, rather than slowing down first. A more advanced implementation
   animates time itself: rather than always adding or subtracting the
   timestep, it interpolates between a factor or -1 and 1.
   (It only does this if the animation is interrupted; if it is restarted
   after completing it works identitcally to the naive version,
   to avoid lag at the start.)

   Click the mouse to trigger the animation. If you cick rapidly,
   the second panel looks a lot better.
-}

import Animation exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onClick, onResize)
import Json.Decode as Decode exposing (Value)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task


second : Float
second =
    1000


width =
    animation 0 |> from 10 |> to 300 |> duration (0.67 * second)


height =
    animation 0 |> from 10 |> to 200 |> duration (0.67 * second) |> delay (0.33 * second)


color =
    animation 0 |> duration second


type RGB
    = RGB Int Int Int


type alias Model =
    { trueClock : Clock

    -- Aristotelian and Newtonian clocks
    , arisClock : Clock
    , newtClock : Clock
    , forward : Bool
    , newtFactor : Animation
    , w : Int
    , h : Int
    }


model0 : Model
model0 =
    Model 0 0 0 False (static -1) 0 0


type Msg
    = Tick Float
    | Resize Int Int
    | Click
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onClick (Decode.succeed Click)
        , onAnimationFrameDelta Tick
        ]


animateRev =
    animation 0 |> from 1 |> to -1 |> duration (0.2 * second)


animateFwd =
    animation 0 |> from -1 |> to 1 |> duration (0.2 * second)


update : Msg -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        Resize w h ->
            { model | w = w, h = h }

        Tick dt ->
            let
                newTrueClock =
                    model.trueClock + dt
            in
            { model
                | trueClock = newTrueClock
                , arisClock =
                    model.arisClock
                        -- always +dt or -dt
                        + (if model.forward then
                            dt

                           else
                            -dt
                          )
                , newtClock =
                    model.newtClock
                        -- often between +dt and -dt
                        + (dt * animate newTrueClock model.newtFactor)
            }

        Click ->
            if model.forward then
                { model
                    | forward = False
                    , newtFactor =
                        if model.newtClock > second then
                            -- skip tweening if restarting animation from rest
                            static -1

                        else
                            animateRev |> delay model.trueClock

                    -- reset clocks that have gotten really big
                    -- but keep them if we're still animating
                    , arisClock = min model.arisClock second
                    , newtClock = min model.newtClock second
                }

            else
                -- works exactly opposite the other case
                { model
                    | forward = True
                    , newtFactor =
                        if model.newtClock < 0 then
                            static 1

                        else
                            animateFwd |> delay model.trueClock
                    , arisClock = max model.arisClock 0
                    , newtClock = max model.newtClock 0
                }


lerp from to v =
    from + (to - from) * v


lerp_ from to v =
    round (lerp (toFloat from) (toFloat to) v)


colorEase : RGB -> RGB -> Float -> RGB
colorEase (RGB r1 g1 b1) (RGB r2 g2 b2) v =
    RGB
        (lerp_ r1 r2 v)
        (lerp_ g1 g2 v)
        (lerp_ b1 b2 v)


padding : Float
padding =
    50


render : Float -> Clock -> Svg Msg
render yOffset clock =
    let
        wid =
            animate clock width

        hei =
            animate clock height

        (RGB r g b) =
            color
                |> animate clock
                |> colorEase (RGB 115 51 128) (RGB 74 178 182)
    in
    Svg.rect
        [ SA.width (String.fromFloat wid)
        , SA.height (String.fromFloat hei)
        , SA.y (String.fromFloat yOffset)
        , SA.fill ("rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")")
        ]
        []


view : Model -> Svg Msg
view { arisClock, newtClock, w, h } =
    Svg.svg
        [ SA.style "position:absolute;left:0;top:0"
        , SA.width (String.fromInt w)
        , SA.height (String.fromInt h)
        ]
        [ Svg.g
            [ SA.transform
                ("translate("
                    ++ String.fromFloat padding
                    ++ " "
                    ++ String.fromFloat padding
                    ++ ")"
                )
            ]
            [ render 0 arisClock
            , render (padding + getTo height) newtClock
            ]
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
        , subscriptions = subscriptions
        , view = view
        }
