module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { count : Int
    }


init : Model
init =
    { count = 0
    }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "card" ]
            [ div [ class "card-body" ]
                [ h2 [ class "card-title" ] [ text "Counter" ]
                , text (String.fromInt model.count)
                , div [ class "card-actions" ]
                    [ button [ class "btn", onClick Decrement ] [ text "-" ]
                    , button [ class "btn", onClick Increment ] [ text "+" ]
                    ]
                ]
            ]
        ]
