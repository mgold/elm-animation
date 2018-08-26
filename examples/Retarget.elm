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
import Html.Events exposing (keyCode)
import Json.Decode as Decode exposing (Decoder, Value)
import Svg exposing (Svg)
import Svg.Attributes as SA
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


subscriptions : Model -> Sub Msg
subscriptions _ =
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
                    { x = toFloat mouseX
                    , y = toFloat mouseY
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
            getVelocity (t - 10) a

        v1 =
            getVelocity (t + 10) a
    in
    (v1 - v0) / 20


view : Model -> Svg Msg
view model =
    Svg.svg
        [ SA.style "position:absolute;left:0;top:0"
        , SA.width (String.fromInt model.w)
        , SA.height (String.fromInt model.h)
        ]
        (List.indexedMap renderClick model.clicks
            ++ List.map renderTrailItem model.trail
            ++ [ renderBall model ]
        )


renderBall : Model -> Svg Msg
renderBall model =
    let
        oneBall =
            renderBall_ model.clock model.x model.y
    in
    if model.smear then
        List.map (\t -> renderBall_ (model.clock + toFloat t * 20) model.x model.y) (List.range -5 5)
            |> Svg.g [ SA.style "opacity: 0.3" ]
            |> (\gr -> Svg.g [] [ oneBall, gr ])

    else
        oneBall


renderBall_ : Clock -> Animation -> Animation -> Svg Msg
renderBall_ clock x y =
    let
        ( posX, posY ) =
            ( animate clock x, animate clock y )

        ( velX, velY ) =
            ( 100 * getVelocity clock x, 100 * getVelocity clock y )

        ( accX, accY ) =
            ( 10000 * acceleration clock x, 10000 * acceleration clock y )
    in
    Svg.g
        [ SA.transform ("translate(" ++ String.fromFloat posX ++ " " ++ String.fromFloat posY ++ ")") ]
        [ Svg.circle [ SA.r "20", SA.fill "lightblue" ] []
        , Svg.line
            [ SA.x2 (String.fromFloat velX)
            , SA.y2 (String.fromFloat velY)
            , SA.stroke "green"
            , SA.strokeWidth "1"
            ]
            []
        , Svg.line
            [ SA.x2 (String.fromFloat accX)
            , SA.y2 (String.fromFloat accY)
            , SA.stroke "red"
            , SA.strokeWidth "1"
            ]
            []
        ]


renderTrailItem : Pos -> Svg Msg
renderTrailItem { x, y } =
    Svg.circle
        [ SA.r "2"
        , SA.fill "orange"
        , SA.cx (String.fromFloat x)
        , SA.cy (String.fromFloat y)
        ]
        []


renderClick : Int -> Pos -> Svg Msg
renderClick i { x, y } =
    Svg.rect
        [ SA.width "12"
        , SA.height "12"
        , SA.x (String.fromFloat (x - 6))
        , SA.y (String.fromFloat (y - 6))
        , SA.fill <|
            if i == 0 then
                "purple"

            else
                "mediumpurple"
        ]
        []


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
