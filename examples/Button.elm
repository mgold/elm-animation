module Button exposing (main)

import Animation exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onClick, onMouseMove, onResize)
import Collage
import Color exposing (darkBlue, white)
import Element exposing (Element)
import Json.Decode as Decode exposing (Decoder, Value)
import Task


type Msg
    = Tick Float
    | MouseMove Int Int
    | MouseClick Int Int
    | Resize Int Int
    | NoOp


{-| State as in a state machine.
-}
type State
    = Entering
    | Here
    | Growing
    | Big
    | Shrinking
    | Exiting
    | Gone


{-| Everything persisted between frames.
-}
type alias Model =
    { w : Int
    , h : Int
    , r : Animation
    , theta : Animation
    , clock : Clock
    , state : State
    }


model0 =
    Model 0 0 (animation 0 |> from 0 |> to here_r) (static (degrees 45)) 0 Entering


dist posX posY =
    let
        x =
            toFloat posX

        y =
            toFloat posY
    in
    sqrt <| x * x + y * y


collided x y { r, clock } =
    animate clock r > dist x y


here_r =
    40


big_r =
    70


update : Msg -> Model -> Model
update act model =
    -- this case analysis is ugly but I think it's intrinsic to the component
    case act of
        Tick t ->
            let
                clock =
                    model.clock + t

                radiusDone =
                    isDone clock model.r

                state =
                    case model.state of
                        Entering ->
                            if radiusDone then
                                Here

                            else
                                Entering

                        Growing ->
                            if radiusDone then
                                Big

                            else
                                Growing

                        Shrinking ->
                            if radiusDone then
                                Here

                            else
                                Shrinking

                        Exiting ->
                            if radiusDone then
                                Gone

                            else
                                Exiting

                        Gone ->
                            if isRunning clock model.r then
                                Entering

                            else
                                Gone

                        _ ->
                            model.state
            in
            if model.state == Gone && radiusDone then
                { model | clock = clock, r = animation clock |> from 0 |> to here_r |> delay 1500 }

            else
                { model | clock = clock, state = state }

        MouseMove mouseX mouseY ->
            let
                posX =
                    mouseX - model.w // 2

                posY =
                    model.h // 2 - mouseY

                now =
                    model.clock

                collision =
                    collided posX posY model

                growingOrBig =
                    model.state == Growing || model.state == Big
            in
            if not collision && growingOrBig then
                { model
                    | r = undo now model.r
                    , theta = undo now model.theta
                    , state = Shrinking
                }

            else if collision && model.state == Here then
                { model
                    | r = retarget now big_r model.r |> duration 150
                    , theta = retarget now (degrees -45) model.theta |> duration 200
                    , state = Growing
                }

            else
                model

        MouseClick mouseX mouseY ->
            let
                posX =
                    mouseX - model.w // 2

                posY =
                    model.h // 2 - mouseY
            in
            if collided posX posY model then
                { model
                    | r = retarget model.clock 0 model.r |> duration 750
                    , theta = retarget model.clock (degrees 45) model.theta |> duration 750
                    , state = Exiting
                }

            else
                model

        Resize width height ->
            { model | w = width, h = height }

        NoOp ->
            model


scene : Model -> Element
scene { w, h, r, theta, clock } =
    let
        radius =
            animate clock r

        angle =
            animate clock theta

        rect =
            Collage.rect (radius / 8) (radius / 1.25) |> Collage.filled white

        circle =
            Collage.circle radius |> Collage.filled darkBlue

        group =
            Collage.group [ circle, rect, rect |> Collage.rotate (degrees 90) ]
                |> Collage.rotate angle
    in
    Collage.collage w h [ group ]


subs : Sub Msg
subs =
    Sub.batch
        [ onResize Resize
        , onAnimationFrameDelta Tick
        , onClick (mousePosition MouseClick)
        , onMouseMove (mousePosition MouseMove)
        ]


mousePosition : (Int -> Int -> Msg) -> Decoder Msg
mousePosition coordsToMsg =
    Decode.map2 coordsToMsg
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)


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
        , view = scene >> Element.toHtml
        }
