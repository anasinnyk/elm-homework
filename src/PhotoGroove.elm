module PhotoGroove exposing (main)

import Html exposing (h1, img, div, text)
import Html.Attributes exposing (..)


-- main : Program Never Model Msg
main = view "no nodel yet"

-- view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnail" ]
            [ img [ src "http://elm-in-action.com/1.jpeg" ] []
            , img [ src "http://elm-in-action.com/2.jpeg" ] []
            , img [ src "http://elm-in-action.com/3.jpeg" ] []
            ]
        ]


