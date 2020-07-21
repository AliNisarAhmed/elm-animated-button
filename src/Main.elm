module Main exposing (..)

import Browser
import ButtonModule exposing (ButtonModel, ButtonMsg, animationMsg, buttonUpdater, createButton, initButtonModel, toHtml)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ButtonMsg btnMsg ->
            let
                ( newButtonModel, btnCommands ) =
                    buttonUpdater btnMsg model.buttonModel
            in
            ( { model | buttonModel = newButtonModel }, Cmd.map ButtonMsg btnCommands )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ css <| [ paddingTop (px 100) ] ]
        [ div
            [ css <|
                [ position relative ]
            ]
            [ toHtml model.buttonModel (createButton "Click me" Nothing (Just <| ButtonMsg animationMsg))
            , toHtml model.buttonModel2 (createButton "Click Me 2" Nothing Nothing)
            ]
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
