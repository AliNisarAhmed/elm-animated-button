module ButtonModule exposing (ButtonModel, ButtonMsg, animationMsg, buttonUpdater, createButton, initButtonModel, toHtml)

import Css exposing (..)
import Css.Animations as CA exposing (keyframes)
import Css.Transitions exposing (cubicBezier, transition)
import Html
import Html.Styled exposing (Attribute, Html, button, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick, onMouseUp)
import Process
import Task


type MyButton msg
    = MyButton (Options msg) String


type alias Options msg =
    { startAnimation : Bool
    , onClick : Maybe msg
    , onMouseUp : Maybe msg
    }


type ButtonMsg
    = Animate
    | Reset


type ButtonModel
    = ButtonModel { animate : Bool }


initButtonModel : ButtonModel
initButtonModel =
    ButtonModel
        { animate = False }


createButton : String -> Maybe msg -> Maybe msg -> MyButton msg
createButton str onClickMsg animateMsg =
    MyButton { defaultOptions | onClick = onClickMsg, onMouseUp = animateMsg } str


animationMsg : ButtonMsg
animationMsg =
    Animate


toHtml : ButtonModel -> MyButton msg -> Html msg
toHtml (ButtonModel model) (MyButton options label) =
    let
        styles =
            buttonStyles model.animate
    in
    case ( options.onClick, options.onMouseUp ) of
        ( Just onClickMsg, Just onMouseUpMsg ) ->
            button [ onClick onClickMsg, onMouseUp onMouseUpMsg, css <| styles ] [ text label ]

        ( Just onClickMsg, Nothing ) ->
            button [ onClick onClickMsg, css <| styles ] [ text label ]

        ( Nothing, Just onMouseUpMsg ) ->
            button [ onMouseUp onMouseUpMsg, css <| styles ] [ text label ]

        ( Nothing, Nothing ) ->
            button [ css <| styles ] [ text label ]


resetCommand : Cmd ButtonMsg
resetCommand =
    Process.sleep 1000.0
        |> Task.andThen (always <| Task.succeed Reset)
        |> Task.perform identity


buttonUpdater : ButtonMsg -> ButtonModel -> ( ButtonModel, Cmd ButtonMsg )
buttonUpdater msg (ButtonModel model) =
    case msg of
        Animate ->
            ( ButtonModel { model | animate = True }, resetCommand )

        Reset ->
            ( ButtonModel { model | animate = False }, Cmd.none )


defaultOptions : Options msg
defaultOptions =
    { startAnimation = True
    , onClick = Nothing
    , onMouseUp = Nothing
    }


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


animation =
    keyframes
        [ ( 50, [ CA.property "transform" "scale(1.1, 1.3)", CA.property "opacity" "0" ] )
        , ( 99, [ CA.property "transform" "scale(0.001, 0.001)", CA.property "opacity" "0" ] )
        , ( 100, [ CA.property "transform" "scale(0.001, 0.001)", CA.property "opacity" "1" ] )
        ]


transitionDuration =
    350
