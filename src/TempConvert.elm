module TempConvert exposing (main)

import Html exposing (Html, text, input, ul, li)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import Browser

type Msg = ChangeC String | ChangeF String

type alias Model = { c : String, f : String }


initModel : Model
initModel = { c  = "", f = "" }

main : Program () Model Msg
main = 
    Browser.element
        { init = \_ -> (initModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
        ChangeF s -> ({ model | c = process f2c s, f = s }, Cmd.none)
        ChangeC s -> ({ model | f = process c2f s, c = s }, Cmd.none)

process : (Float -> Float) -> String -> String
process f s = case String.toFloat s of
                Just d  -> String.fromFloat (f d)
                Nothing -> s

f2c : Float -> Float
f2c d = (d - 32) * 5 / 9

c2f : Float -> Float
c2f d = d * (9/5) + 32

view : Model -> Html Msg
view model =
    ul [] [ li [] [ text "C"
                    , input [ type_ "number", value model.c, onInput (\s -> ChangeC s) ] []
                    ]
            , li [] [ text "F"
                    , input [ type_ "number", value model.f, onInput (\s -> ChangeF s) ] [] 
                    ] 
            ]
