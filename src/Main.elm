module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Animations as CA exposing (keyframes)
import Css.Transitions exposing (cubicBezier, transition)
import Html
import Html.Styled exposing (Attribute, Html, button, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick, onMouseUp)
import Process
import Task



---- MODEL ----


type alias Model =
    { value : String
    , animate : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { value = "", animate = False }, Cmd.none )



---- UPDATE ----


type Msg
    = ButtonClicked
    | Animate
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ButtonClicked ->
            ( { model | value = "Button Clicked" }, resetCommand )

        Animate ->
            ( { model | animate = True }, Cmd.none )

        Reset ->
            ( { model | animate = False }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ css <| [ paddingTop (px 100) ] ]
        [ div
            [ css <|
                [ position relative ]
            ]
            [ button (buttonAttr model.animate) [ text "Click Me" ]
            , text model.value
            ]
        ]


buttonAttr : Bool -> List (Attribute Msg)
buttonAttr animate =
    [ onClick ButtonClicked, onMouseUp Animate, css <| buttonStyles animate ]


transitionDuration =
    350


buttonStyles : Bool -> List Style
buttonStyles animate =
    let
        baseAttributes =
            [ borderRadius (px 2)
            , padding2 (px 4) (px 15)
            , borderWidth (px 1)
            , fontSize (px 14)
            , height (px 34)
            , outline none
            ]

        defaultButtonAttributes =
            [ position relative
            , color (hex primaryColor)
            , borderStyle solid
            , backgroundColor (hex "#fff")
            , borderColor <| rgb 217 217 217
            , focus
                [ borderColor (hex primaryColorFaded)
                , color (hex primaryColorFaded)
                ]
            , hover
                [ borderColor (hex primaryColorFaded)
                , color (hex primaryColorFaded)
                ]
            , active
                [ borderColor (hex primaryColor)
                , color (hex primaryColor)
                ]
            , transition
                [ Css.Transitions.borderColor transitionDuration
                , Css.Transitions.color transitionDuration
                ]
            ]
    in
    if animate == False then
        baseAttributes ++ defaultButtonAttributes

    else
        baseAttributes
            ++ defaultButtonAttributes
            ++ [ position relative
               , borderColor (hex primaryColor)
               , color (hex primaryColor)
               , animatedBefore
               ]


animatedBefore : Style
animatedBefore =
    before
        [ property "content" "\" \""
        , display block
        , position absolute
        , width (pct 100)
        , height (pct 100)
        , right (px 0)
        , left (px 0)
        , top (px 0)
        , bottom (px 0)
        , borderRadius (px 5)
        , backgroundColor <| hex primaryColor
        , transition [ clickTransition ]
        , zIndex (int -1)
        , animationName animation
        , animationDuration (sec 2)
        , property "animation-timing-function" "cubic-bezier(0.08, 0.82, 0.17, 1)"
        , property "animation-fill-mode" "forwards"
        , animationIterationCount (int 1)
        ]


clickTransition =
    Css.Transitions.transform3 2000 2000 <| cubicBezier 0.08 0.82 0.17 1


resetCommand : Cmd Msg
resetCommand =
    Process.sleep 1000.0
        |> Task.andThen (always <| Task.succeed Reset)
        |> Task.perform identity



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


primaryColor : String
primaryColor =
    "#1890ff"


primaryColorFaded : String
primaryColorFaded =
    "#40a9ff"


primaryColorStrong : String
primaryColorStrong =
    "#096dd9"


warningColor : String
warningColor =
    "#faad14"


dangerColor : String
dangerColor =
    "#ff4d4f"



-- @keyframes waveEffect {
--   100% {
--     box-shadow: 0 0 0 @primary-color;
--     box-shadow: 0 0 0 @wave-animation-width var(--antd-wave-shadow-color);
--   }
-- }
-- @keyframes fadeEffect {
--   100% {
--     opacity: 0;
--   }
-- }


animation =
    keyframes
        [ ( 50, [ CA.property "transform" "scale(1.1, 1.3)", CA.property "opacity" "0" ] )
        , ( 99, [ CA.property "transform" "scale(0.001, 0.001)", CA.property "opacity" "0" ] )
        , ( 100, [ CA.property "transform" "scale(0.001, 0.001)", CA.property "opacity" "1" ] )
        ]
