module Timer exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)


type Msg
    = ChangeC String
    | ChangeF String


type alias Model =
    { time : String, percent : String }


initModel : Model
initModel =
    { time = "0", percent = "0%" }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = Debug.todo "update"


view : Model -> Html Msg
view model =
    div []
        [ text "Elapsed Time: "
        , div [ classList [ ( "mdl-progress", True ), ( "mdl-js-progress", True ) ] ] []
        , div [] [ text (model.time ++ "s") ]
        , text "Duration: "
        , input [ classList [ ( "mdl-slider", True ), ( "mdl-js-slider", True ) ], type_ "range", Html.Attributes.min "0", Html.Attributes.max "100", value model.time ] []
        , button [ classList [ ( "mdl-button", True ), ( "mdl-js-button", True ), ( "mdl-button--raised", True ), ( "mdl-js-ripple-effect", True ), ( "mdl-button--accent", True ) ] ] [ text "Reset" ]
        ]
