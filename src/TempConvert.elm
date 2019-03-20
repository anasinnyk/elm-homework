module TempConvert exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type Msg
    = ChangeC String
    | ChangeF String


type alias Model =
    { c : String, f : String }


initModel : Model
initModel =
    { c = "", f = "" }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeF s ->
            ( { model | c = process f2c s, f = s }, Cmd.none )

        ChangeC s ->
            ( { model | f = process c2f s, c = s }, Cmd.none )


process : (Float -> Float) -> String -> String
process f s =
    case String.toFloat s of
        Just d ->
            String.fromFloat (f d)

        Nothing ->
            s


f2c : Float -> Float
f2c d =
    (d - 32) * 5 / 9


c2f : Float -> Float
c2f d =
    d * (9 / 5) + 32

inputHtml v l action= div [ classList [ ("mdl-textfield", True), ("mdl-js-textfield", True), ("mdl-textfield--floating-label", True) ] ]
            [ input
                [ class "mdl-textfield__input"
                , type_ "number"
                , value v
                , id l
                , onInput action
                ]
                []
            , label
                [ class "mdl-textfield__label"
                , for l
                ]
                [ text l ]
            ]

view : Model -> Html Msg
view model =
    Html.form [ action "#" ]
        [ inputHtml model.c "celcius" ChangeC
        , text " = "
        , inputHtml model.f "fahrenheit" ChangeF
        ]