module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Matrix exposing (..)
import List exposing (..)
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
    | GameOver Gameboard

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
    , playedNumbers : List (Int)
    }

--5x5 gameboard initialization
gameboard : Gameboard
gameboard = 
    { gameMatrix = 
        matrix 5 5 (\location -> initSquare 0)
    , gameNumbers = List.range 1 75
    , gameboardNumbers = List.range 1 75
    , playedNumbers = []
    }

--used for checking the diagonal rows
gameboardDiagonals : List Location
gameboardDiagonals =
    [loc 0 0, loc 1 1, loc 2 2, loc 3 3, loc 4 4 , loc 4 0, loc 3 1, loc 2 2, loc 1 3, loc 0 4]

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
        [ 
            div [ class "square" ]
            [
            button [classList [("squareButton", True),("pressed", square.pressed)], onClick (GameboardClick square) ] [text (toString square.number)]
            ]    
        ]
--split list into list of lists

split : Int -> List Int -> List (List Int)
split n list =
  case take n list of
    [] -> []
    listHead -> listHead :: split n (drop n list)

--function for playing the next number

playNextNumber : Model -> (Model, Cmd Msg)
playNextNumber model =
    case model of
        BeginGame ->
            model ! []

        Playing gameboard ->
            let
                newGameboard = gameboard
            
            in
                Playing {newGameboard |
                        gameNumbers = drop 1 gameboard.gameNumbers,   
                        playedNumbers = 
                            take 1 gameboard.gameNumbers
                            |> append gameboard.playedNumbers
                    } ! []
        GameOver gameboard ->
            model ! []

--shows played numbers

showPlayedNumbers : List (Int) -> Html Msg
showPlayedNumbers list = 
    ul[class "played-numbers"] (List.map (\l -> li[][text (toString l)]) list)

--checks if player has 5 in a row

checkWin : Square -> Model -> (Model, Cmd Msg)
checkWin sqr model =
    case model of
        BeginGame ->
            model ! []
            
        Playing gameboard ->
            let 
                newGameboard = gameboard
                newMatrix =
                    Matrix.map (pressSquare True gameboard sqr) gameboard.gameMatrix
            in
                checkWinHelper {newGameboard |
                        gameMatrix = newMatrix
                } ! []
        
        GameOver gameboard-> 
            model ! []

checkWinHelper : Gameboard -> Model
checkWinHelper gmbrd =
    if (checkRows gmbrd) 
        || (checkRows {gmbrd | gameMatrix = fromList (transpose (toList gmbrd.gameMatrix))})
        || (checkDiagonals gmbrd) 
        then    
        GameOver gmbrd
    
    else 
        Playing gmbrd

--function for checking rows in gameboard
checkRows : Gameboard -> Bool
checkRows gmbrd =
    let 
        listOfLists = toList gmbrd.gameMatrix
        firstList = concat (take 1 listOfLists)
    in
        if (firstList == []) then
            False
        else if (length (List.filter (\x -> (List.any (\y -> y == x.number) gmbrd.playedNumbers) && x.pressed) firstList) == 5) then
            True
        else
            checkRows {gmbrd |
                gameMatrix = fromList (drop 1 listOfLists)}

-- turns cols to rows so we can use the previous function again    
transpose: List (List a) -> List (List a) 
transpose listOfLists =
    case listOfLists of
        [] ->
        []

        ([] :: xss) ->
        transpose xss

        ((x::xs) :: xss) ->
        let
            heads =
            List.filterMap List.head xss

            tails =
            List.filterMap List.tail xss
        in
            (x :: heads) :: transpose (xs :: tails)

--checks diagonal rows    
checkDiagonals : Gameboard -> Bool
checkDiagonals gmbrd =  
    let
        playedNums = gmbrd.playedNumbers
        matrix = gmbrd.gameMatrix
        sqrListOne = List.map (locToSquare matrix) (take 5 gameboardDiagonals)
        sqrListTwo = List.map (locToSquare matrix) (drop 5 gameboardDiagonals)
        
    in
        if (sqrListOne == [] || sqrListTwo == []) then
            False
        else if (length (List.filter (\x -> (List.any (\y -> y == x.number) playedNums) && x.pressed) sqrListOne) == 5) then
            True
        else if (length (List.filter (\x -> (List.any (\y -> y == x.number) playedNums) && x.pressed) sqrListTwo) == 5) then
            True
        else 
            False

--helper function for checkDiagonals
locToSquare : Matrix Square -> Location -> Square
locToSquare matrix loc =
    case Matrix.get loc matrix of
        Just a ->
            a
        Nothing ->
            initSquare -1


-- changes Squares pressed value from False to True when used with map 
pressSquare : Bool -> Gameboard -> Square -> Square -> Square
pressSquare isPressed gmbrd a b =
    if ((a.number == b.number) && (List.any (\x -> (x == a.number)) gmbrd.playedNumbers)) then
        {b | pressed = isPressed}
    else
        b


createModel : ( Model, Cmd Msg )
createModel =
    BeginGame ! []

type Msg
    = NoOp |
    StartGame |
    Shuffle (List (Int), List (Int)) |
    NextNumber |
    GameboardClick Square

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

        NextNumber ->
            playNextNumber model

        GameboardClick sqr->
            if (not sqr.pressed) then
                checkWin sqr model
            else
                model ! []

wrapper : Gameboard -> Html Msg -> Html Msg
wrapper gmbrd overlay = 
    div[class "site-wrapper"][
                h1[][text "Your gameboard"],
                br[][],
                div [class "container"] (flatten (Matrix.map createSquare gmbrd.gameMatrix)),
                button [onClick NextNumber][text "Next number"],
                showPlayedNumbers gmbrd.playedNumbers,
                overlay

            ]

playAgainOverlay : Html Msg
playAgainOverlay = 
    div[class "congratulations"][
                h1[][text "You won!"],
                button [onClick StartGame][text "Want to play again?"] 
            ]

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
            wrapper gameboard (text "")

        GameOver gameboard ->
            wrapper gameboard playAgainOverlay
            