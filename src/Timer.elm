module Timer exposing (main)

import Browser
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode
import Task
import Time


type Msg
    = GotInitialTime Time.Posix
    | TimeUpdate Time.Posix
    | ChnageTime Float
    | TimeError


type alias Model =
    { time : Float }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 200.0 TimeUpdate


initModel : Model
initModel =
    { time = 0 }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInitialTime t ->
            ( model, Task.perform GotInitialTime Time.now )

        TimeUpdate t ->
            ( { time =
                    if model.time <= 0 then
                        0

                    else
                        model.time - 0.2
              }
            , Cmd.none
            )

        ChnageTime t ->
            ( { time =
                    if t <= 0 then
                        0

                    else
                        t
              }
            , Cmd.none
            )

        TimeError ->
            ( model, Cmd.none )


sharesLocale : Locale
sharesLocale =
    { usLocale | decimals = 2, negativePrefix = "" }


view : Model -> Html Msg
view model =
    div [ classList [ ( "container", True ) ] ]
        [ div [ classList [ ( "row", True ) ] ]
            [ div [ classList [ ( "col-sm", True ) ] ] []
            , div [ classList [ ( "col-sm", True ) ] ]
                [ text "Elapsed Time: "
                , div [ classList [ ( "progress", True ) ] ]
                    [ div
                        [ classList [ ( "progress-bar", True ) ]
                        , attribute "role" "progressbar"
                        , attribute "aria-valuenow" <| String.fromInt <| round model.time * 10
                        , attribute "aria-valuemin" "0"
                        , attribute "aria-valuemax" "100"
                        , style "width" ((String.fromInt <| round model.time * 10) ++ "%")
                        ]
                        []
                    ]
                , div [] [ text (format sharesLocale model.time ++ "s") ]
                , label [ for "range" ] [ text "Duration:" ]
                , input
                    [ classList [ ( "custom-range", True ) ]
                    , id "range"
                    , type_ "range"
                    , Html.Attributes.min "0"
                    , Html.Attributes.max "10"
                    , step "0.2"
                    , value <| String.fromFloat model.time
                    , onInput
                        (\s ->
                            case String.toFloat s of
                                Just v ->
                                    ChnageTime v

                                Nothing ->
                                    TimeError
                        )
                    ]
                    []
                , button
                    [ classList
                        [ ( "btn", True )
                        , ( "btn-primary", True )
                        ]
                    , onClick <| ChnageTime 0
                    ]
                    [ text "Reset" ]
                ]
            , div [ classList [ ( "col-sm", True ) ] ] []
            ]
        ]
