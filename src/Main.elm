module Main exposing (..)

import Browser
import ButtonModule
    exposing
        ( ButtonModel
        , ButtonMsg
        , animationMsg
        , buttonUpdater
        , createNormalButton
        , initButtonModel
        , toHtml
        , withAnimation
        )
import Css exposing (..)
import Html
import Html.Styled exposing (Attribute, Html, button, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick, onMouseUp)



---- MODEL ----


type alias Model =
    { value : String
    , buttonModel : ButtonModel
    , buttonModel2 : ButtonModel
    }


init : ( Model, Cmd Msg )
init =
    ( { value = "", buttonModel = initButtonModel, buttonModel2 = initButtonModel }, Cmd.none )



---- UPDATE ----


type Msg
    = ButtonMsg ButtonMsg
    | ButtonMsg2 ButtonMsg
    | NormalButtonClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ButtonMsg btnMsg ->
            let
                ( newButtonModel, btnCommands ) =
                    buttonUpdater btnMsg model.buttonModel
            in
            ( { model | buttonModel = newButtonModel }, Cmd.map ButtonMsg btnCommands )

        ButtonMsg2 btnMsg ->
            let
                ( newButtonModel, btnCommands ) =
                    buttonUpdater btnMsg model.buttonModel2
            in
            ( { model | buttonModel2 = newButtonModel }, Cmd.map ButtonMsg btnCommands )

        NormalButtonClicked ->
            ( { model | value = "Button Clicked" }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ css <| [ paddingTop (px 100) ] ]
        [ div
            [ css <|
                [ position relative ]
            ]
            [ toHtml (createNormalButton "Click me" <| Just NormalButtonClicked)
            ]
        , div
            [ css <|
                [ position relative ]
            ]
            [ toHtml (createNormalButton "Animated onClick" Nothing |> withAnimation (ButtonMsg animationMsg) model.buttonModel)
            ]
        , div
            [ css <|
                [ position relative ]
            ]
            [ toHtml (createNormalButton "Animated onClick2" Nothing |> withAnimation (ButtonMsg2 animationMsg) model.buttonModel2)
            ]
        , div
            []
            [ text model.value ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
