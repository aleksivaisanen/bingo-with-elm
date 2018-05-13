module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = createModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    {}


type Msg
    = NoOp

--creates one square to gameboard
createSquare : Html Msg
createSquare =
    div [ class "container" ]
        [ div [ classList [ ( "pressed", False ) ] ]
            [ div [ class "square" ] []
            ]
        ]

createModel : ( Model, Cmd Msg )
createModel =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [class "wrapper" ]
        [createSquare
        , createSquare
        , createSquare
        ]