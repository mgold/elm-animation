module Retarget exposing (main)

{- A blue ball moves towards wherever you click.

   Shown onscreen:

   - Blue ball: current position of object
   - Green line: velocity vector
   - Red line: acceleration vector
   - Orange dots: location sampled at regular intervals
   - Purple squares: destinations (current one is darker)

   Controls:

   - Click to set destination (including while ball is in motion)
   - Shift to slow down time
   - Space to see the interpolated past and future
   - Enter to clear the history

   Notice:

   - When not retargeted, the velocity and acceleration vectors
     are in the line of travel.
   - When retargeted, acceleration changes instantly but
     velocity does not.
   - When retargeted, the interpolated past and future change
     but the present does not.
   - Each trip takes a constant duration, meaning shorter trips
     are slower. One could instead specify speed.
-}

import Animation exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onKeyUp, onResize)
import Collage as C exposing (Form)
import Color exposing (Color)
import Element as E exposing (Element)
import Html.Events exposing (keyCode)
import Json.Decode as Decode exposing (Decoder, Values)
import Task


millisecond : Float
millisecond =
    1


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Model =
    { clock : Clock
    , x : Animation
    , y : Animation
    , w : Int
    , h : Int
    , trail : List Pos
    , clicks : List Pos
    , lastClickTime : Clock
    , slow : Bool
    , smear : Bool
    }


dur =
    750


model0 : Model
model0 =
    Model 0 (static 0) (static 0) 0 0 [] [] 0 False False


type Msg
    = Tick Float
    | Resize Int Int
    | Click Int Int
    | Reset
    | NoOp
    | Slow Bool
    | Smear Bool


subs : Sub Msg
subs =
    Sub.batch
        [ onKeyDown (keyboardMsg True)
        , onKeyUp (keyboardMsg False)
        , onClick mousePosition
        , onResize Resize
        , onAnimationFrameDelta Tick
        ]


keyboardMsg : Bool -> Decoder Msg
keyboardMsg isDown =
    Decode.map
        (\code ->
            case code of
                -- shift key
                16 ->
                    Slow isDown

                -- space key
                32 ->
                    Smear isDown

                _ ->
                    Reset
        )
        keyCode


mousePosition : Decoder Msg
mousePosition =
    Decode.map2 Click
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)


update : Msg -> Model -> Model
update action model =
    case action of
        Tick dt ->
            updateTick dt model

        Resize width height ->
            { model | w = width, h = height }

        Click mouseX mouseY ->
            let
                pos =
                    { x = toFloat mouseX - toFloat model.w / 2
                    , y = toFloat model.h / 2 - toFloat mouseY
                    }
            in
            { model
                | clicks =
                    pos :: model.clicks

                --                 sync durations, very important
                , x = retarget model.clock pos.x model.x |> duration dur
                , y = retarget model.clock pos.y model.y |> duration dur
            }

        Reset ->
            { model
                | clicks = List.head model.clicks |> Maybe.map (\c -> [ c ]) |> Maybe.withDefault []
                , trail = []
            }

        Slow b ->
            { model | slow = b }

        Smear b ->
            { model | smear = b }

        NoOp ->
            model


close a b =
    abs (a - b) < 1


updateTick : Float -> Model -> Model
updateTick dt model =
    let
        clock =
            model.clock
                + (if model.slow then
                    dt / 5

                   else
                    dt
                  )

        pos =
            { x = animate clock model.x, y = animate clock model.y }

        trail_dt =
            millisecond * 30

        recentlyClicked =
            model.lastClickTime + trail_dt > clock

        lastClickTime =
            if recentlyClicked then
                model.lastClickTime

            else
                model.lastClickTime + trail_dt

        trail =
            case List.head model.trail of
                Nothing ->
                    [ pos ]

                Just pos_ ->
                    if close pos.x pos_.x && close pos.y pos_.y || recentlyClicked then
                        model.trail
                        -- find the position for the time of the dot rather than using the current one

                    else
                        { x = animate lastClickTime model.x, y = animate lastClickTime model.y } :: model.trail
    in
    { model | clock = clock, trail = trail, lastClickTime = lastClickTime }


acceleration : Float -> Animation -> Float
acceleration t a =
    let
        v0 =
            velocity (t - 10) a

        v1 =
            velocity (t + 10) a
    in
    (v1 - v0) / 20


render : Model -> Element
render model =
    C.collage model.w model.h <|
        renderClicks model
            ++ renderTrail model
            ++ [ renderBall model ]


renderBall model =
    let
        oneBall =
            renderBall_ model.clock model.x model.y
    in
    if model.smear then
        List.map (\t -> renderBall_ (model.clock + toFloat t * 20) model.x model.y) (List.range -5 5)
            |> C.group
            |> C.alpha 0.3
            |> (\gr -> C.group [ oneBall, gr ])

    else
        oneBall


renderBall_ clock x y =
    let
        pos =
            ( animate clock x, animate clock y )

        vel =
            ( 100 * velocity clock x, 100 * velocity clock y )

        acc =
            ( 10000 * acceleration clock x, 10000 * acceleration clock y )
    in
    C.group
        [ C.circle 20 |> C.filled Color.darkBlue
        , C.segment ( 0, 0 ) vel |> thick Color.green
        , C.segment ( 0, 0 ) acc |> thick Color.red
        ]
        |> C.move pos


renderTrail { trail } =
    List.map
        (\{ x, y } -> C.circle 2 |> C.filled Color.lightOrange |> C.move ( x, y ))
        trail


renderClicks { clicks } =
    List.indexedMap
        (\i { x, y } ->
            C.square 12
                |> C.filled
                    (if i == 0 then
                        Color.purple

                     else
                        Color.lightPurple
                    )
                |> C.move ( x, y )
        )
        clicks


thick : Color -> C.Path -> Form
thick c =
    let
        style =
            C.solid c
    in
    C.traced { style | width = 2 }


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
        , view = render >> E.toHtml
        }
