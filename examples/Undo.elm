module Undo exposing (main)

{- This example demonstrates the need to set the easing function
   of undone animations as `(\t -> 1 - (a.ease (1 - t)))`.

   Otherwise, undone animations will appear to work for symmetrical
   easing functions but not for asymmetric ones. The outer `1 -`
   is necessary so that the `from` and `to` values may be set correctly,
   as they may be inspected.
-}

import Animation exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Json.Decode exposing (Value)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task


{-| Asymmetric ease in cubic
-}
myEase x =
    x * x * x


type alias Model =
    { w : Int
    , h : Int
    , x : Animation
    , clock : Clock
    , undone : Bool
    , dotsOut : List Float
    , dotsBack : List Float
    }


type Msg
    = Resize Int Int
    | Tick Float


model0 =
    Model 0 0 (animation 0 |> duration 4000 |> ease myEase) 0 False [] []


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            let
                clock =
                    model.clock + dt

                pos =
                    animate clock model.x
            in
            if not model.undone && pos > 0.999 then
                { model | x = undo clock model.x, undone = True, dotsOut = pos :: model.dotsOut, dotsBack = [ pos ], clock = clock }

            else if not model.undone then
                { model | dotsOut = pos :: model.dotsOut, clock = clock }

            else
                { model | dotsBack = pos :: model.dotsBack, clock = clock }

        Resize width height ->
            { model | w = width, h = height }


view : Model -> Svg Msg
view model =
    let
        circle =
            Svg.circle
                [ SA.r "8"
                , SA.fill "purple"
                , SA.cx (String.fromFloat (toFloat model.w * animate model.clock model.x - toFloat (model.w // 2)))
                , SA.cy <|
                    if model.undone then
                        "-30"

                    else
                        "0"
                ]
                []

        dotsOut =
            List.map
                (\x ->
                    Svg.circle
                        [ SA.r "3"
                        , SA.fill "mediumpurple"
                        , SA.cx (String.fromFloat (toFloat model.w * x - toFloat (model.w // 2)))
                        ]
                        []
                )
                model.dotsOut

        dotsBack =
            List.map
                (\x ->
                    Svg.circle
                        [ SA.r "3"
                        , SA.fill "indigo"
                        , SA.cx (String.fromFloat (toFloat model.w * x - toFloat (model.w // 2)))
                        , SA.cy "-30"
                        ]
                        []
                )
                model.dotsBack

        group =
            Svg.g
                [ SA.transform
                    ("translate("
                        ++ String.fromInt (model.w // 2)
                        ++ " "
                        ++ String.fromInt (model.h // 2)
                        ++ ")"
                    )
                ]
                (circle :: dotsOut ++ dotsBack)
    in
    Svg.svg
        [ SA.style "position:absolute;left:0;top:0"
        , SA.width (String.fromInt model.w)
        , SA.height (String.fromInt model.h)
        ]
        [ group ]


subscriptions : Model -> Sub Msg
subscriptions _ =
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
        , subscriptions = subscriptions
        , view = view
        }
