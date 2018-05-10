module Bingo exposing (main)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)


-- MODEL

type alias Model = 
  { list : List Int
  }

model : Model  
model = 
  { list = List.range 1 75 
  }

-- UPDATE

type Msg
    = ShuffleIt
    | ShuffledList (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleIt ->
            ( model, generate ShuffledList (shuffle model.list) )

        ShuffledList shuffledList ->
            { model | list = shuffledList } ! []


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick ShuffleIt ] [ text "New game" ]
    , div [] [ text (toString model.list) ]
    ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }


    