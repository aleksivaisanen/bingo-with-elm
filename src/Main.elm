module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Matrix exposing (..)
import List exposing (take, map, head, drop)
import Random exposing (generate, pair, Generator)
import Random.List exposing (shuffle)



main : Program Never Model Msg
main =
    Html.program
        { init = createModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Model =
    BeginGame
    | Playing Gameboard

--square type for the gameboard
type alias Square = 
    { number : Int
    , pressed : Bool
    }

--gameboard, implemented using matrix
type alias Gameboard =
    { gameMatrix : Matrix Square
    , gameNumbers : List (Int)
    , gameboardNumbers : List (Int)
    }

type Msg
    = NoOp |
    StartGame |
    Shuffle (List (Int), List (Int))

--5x5 gameboard initialization
gameboard : Gameboard
gameboard = 
    { gameMatrix = 
        matrix 5 5 (\location -> initSquare 0)
    , gameNumbers = List.range 1 75
    , gameboardNumbers = List.range 1 75
    }

--takes to lists and returns pair generator for shuffling both lists 
twoRandomLists :List Int -> List Int -> Generator (List Int, List Int)
twoRandomLists list listTwo=
    pair (shuffle list) (shuffle listTwo)

--generates a Cmd Msg to shuffle the list
randomList : Cmd Msg
randomList =
    generate Shuffle (twoRandomLists gameboard.gameNumbers gameboard.gameboardNumbers)

--initializes one square
initSquare : Int  -> Square
initSquare int =
    { number = int
    , pressed = False
    }

--creates one square in html to gameboard
createSquare : Square -> Html Msg
createSquare square =
    div [ class "squareContainer" ]
        [ div [ classList [ ( "pressed", False ) ] ]
            [ div [ class "square" ] [
                button [ class "squareButton" ] [text (toString square.number)]
            ]
            ]
        ]
--split list into list of lists

split : Int -> List Int -> List (List Int)
split n list =
  case take n list of
    [] -> []
    listHead -> listHead :: split n (drop n list)

createModel : ( Model, Cmd Msg )
createModel =
    BeginGame ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        NoOp ->
            model ! []
        
        StartGame ->
            (model, randomList)

        Shuffle (shuffledListOne, shuffledListTwo) ->
            let
                newGameboardNumbers = take 25 shuffledListTwo 
                listOfLists = split 5 newGameboardNumbers

            in
                Playing { gameboard |
                    gameMatrix = Matrix.map initSquare (fromList listOfLists), 
                    gameNumbers = shuffledListOne, 
                    gameboardNumbers = take 25 shuffledListTwo } ! []



view : Model -> Html Msg
view model =
    case model of
        BeginGame ->
            div[class "site-wrapper"][
                h1[][text "Elm Bingo!"],
                h2[][text "Do you want to start the game?"],
                button [onClick StartGame][text "Start game!"],
                br[][],
                br[][],
                br[][],
                p[][text "Aleksi Väisänen 2018"]
            ]

        Playing gameboard ->
            div[class "site-wrapper"][
                h1[][text "Your gameboard"],
                br[][],
                div [class "container"] (flatten (Matrix.map createSquare gameboard.gameMatrix))

            ]