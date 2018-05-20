module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Matrix exposing (..)
import List exposing (..)
import Random exposing (generate, pair, Generator)
import Random.List exposing (shuffle)
import Random.Extra exposing (sample)



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
    | Playing Gameboard GameNumbers
    | GameOver Gameboard GameNumbers Int

--square type for the gameboard
type alias Square = 
    { number : Int
    , pressed : Bool
    }

--gameboard, implemented using matrix
type alias Gameboard =
    { gameMatrix : Matrix Square
    , gameboardNumbers : List (Int)
    }

--5x5 gameboard initialization
gameboard : Gameboard
gameboard = 
    { gameMatrix = matrix 5 5 (\location -> initSquare 0)
    , gameboardNumbers = List.range 1 75
    }

-- gamenumbers to be played with
type alias GameNumbers =
    { toBePlayed : List (Int)
    , playedNums : List (Int)
    }

gameNumbers : GameNumbers
gameNumbers =
    { toBePlayed = List.range 1 75
    , playedNums = []
    }

--used for checking the diagonal rows
gameboardDiagonals : List Location
gameboardDiagonals =
    [loc 0 0, loc 1 1, loc 2 2, loc 3 3, loc 4 4 , loc 4 0, loc 3 1, loc 2 2, loc 1 3, loc 0 4]

--takes to lists and returns pair generator for shuffling both lists 
manyRandomLists :List (List Int) -> Cmd Msg
manyRandomLists listOfLists =
    Cmd.batch (List.map randomList listOfLists)


--generates a Cmd Msg to shuffle the list
randomList : List Int -> Cmd Msg
randomList list =
    generate Shuffle (shuffle list)

--generates a Cmd Msg to take the next random playable number
randomizeNextGameNumber : List (Int) -> Cmd Msg
randomizeNextGameNumber list =
    generate RandomizeNextGameNumber (Random.Extra.sample list)

--initializes one square with 0 as already pressed
initSquare : Int  -> Square
initSquare int =
    if (int == 0) then
        { number = int
        , pressed = True
        }
    else
        { number = int
        , pressed = False
        }

--split list into list of lists

split : Int -> List Int -> List (List Int)
split n list =
  case take n list of
    [] -> []
    listHead -> listHead :: split n (drop n list)

--function for playing the next number

playNextNumber : Model -> Int -> (Model, Cmd Msg)
playNextNumber model num =
    case model of
        BeginGame ->
            model ! []

        Playing gameboard gameNums->
            let
                newGameNums = gameNums
                nextRandomNum = num
            in
                Playing gameboard
                    {newGameNums |
                        toBePlayed = filter (\x -> not (x == nextRandomNum )) gameNums.toBePlayed  
                        , playedNums = append gameNums.playedNums [nextRandomNum]
                    } ! []

        GameOver gameboard gameNums _ ->
            model ! []

--checks if player has 5 in a row

checkWin : Square -> Model -> (Model, Cmd Msg)
checkWin sqr model =
    case model of
        BeginGame ->
            model ! []
            
        Playing gameboard gameNums->
            let 
                newGameboard = gameboard
                newMatrix =
                    Matrix.map (pressSquare True gameboard gameNums sqr) gameboard.gameMatrix
            in
                checkWinHelper {newGameboard |
                        gameMatrix = newMatrix
                } gameNums ! []
        
        GameOver gameboard gameNums _ -> 
            model ! []

checkWinHelper : Gameboard -> GameNumbers ->Model
checkWinHelper gmbrd gameNums =
    if (checkRows gmbrd gameNums) 
        || (checkRows {gmbrd | gameMatrix = fromList (transpose (toList gmbrd.gameMatrix))} gameNums)
        || (checkDiagonals gmbrd gameNums) 
        then    
        GameOver gmbrd gameNums (length gameNums.playedNums)
    
    else 
        Playing gmbrd gameNums

--function for checking rows in gameboard
checkRows : Gameboard -> GameNumbers -> Bool
checkRows gmbrd gameNums =
    let 
        listOfLists = toList gmbrd.gameMatrix
        firstList = concat (take 1 listOfLists)
    in
        if (firstList == []) then
            False
        else if (length (List.filter (\x -> (List.any (\y -> y == x.number) (0 :: gameNums.playedNums)) && x.pressed) firstList) == 5) then
            True
        else
            checkRows {gmbrd |
                gameMatrix = fromList (drop 1 listOfLists)}
                gameNums

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
checkDiagonals : Gameboard -> GameNumbers -> Bool
checkDiagonals gmbrd gameNums =  
    let
        playedNums = gameNums.playedNums
        matrix = gmbrd.gameMatrix
        sqrListOne = List.map (locToSquare matrix) (take 5 gameboardDiagonals)
        sqrListTwo = List.map (locToSquare matrix) (drop 5 gameboardDiagonals)
        
    in
        if (sqrListOne == [] || sqrListTwo == []) then
            False
        else if (length (List.filter (\x -> (List.any (\y -> y == x.number) (0 :: playedNums)) && x.pressed) sqrListOne) == 5) then
            True
        else if (length (List.filter (\x -> (List.any (\y -> y == x.number) (0 :: playedNums)) && x.pressed) sqrListTwo) == 5) then
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
pressSquare : Bool -> Gameboard -> GameNumbers -> Square -> Square -> Square
pressSquare isPressed gmbrd gameNums a b =
    if ((a.number == b.number) && (List.any (\x -> (x == a.number)) gameNums.playedNums)) then
        {b | pressed = isPressed}
    else
        b


createModel : ( Model, Cmd Msg )
createModel =
    BeginGame ! []

type Msg
    = NoOp |
    StartGame |
    Shuffle (List (Int)) |
    RandomizeNextGameNumber (Maybe Int) |
    NextNumber |
    GameboardClick Square

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        NoOp ->
            model ! []
        
        StartGame ->
            (model, manyRandomLists [gameboard.gameboardNumbers])

        Shuffle (list) ->
            let
                newGameboardNumbers = take 25 list 
                matrixLists = split 5 newGameboardNumbers
            in
                Playing { gameboard |
                    gameMatrix = Matrix.set (loc 2 2) (initSquare 0) (Matrix.map initSquare (fromList matrixLists)) 
                } gameNumbers ! []
        
        RandomizeNextGameNumber (num) ->
            case num of
                Nothing ->
                    model ! []
                Just number ->
                    playNextNumber model number
                
        NextNumber ->
            case model of
                BeginGame ->
                    model ! []
                Playing gameboard gameNumbers ->
                    (model, (randomizeNextGameNumber gameNumbers.toBePlayed))
                GameOver gameboard gameNumbers int ->
                    model ! []
            

        GameboardClick sqr->
            if (not sqr.pressed) then
                checkWin sqr model
            else
                model ! []

--all the html stuff 

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

--shows played numbers

showPlayedNumbers : List (Int) -> Html Msg
showPlayedNumbers list = 
    ul[class "played-numbers"] (List.map (\l -> li[class "played-number-item"][text (toString l)]) list)

wrapper : Gameboard -> GameNumbers -> Html Msg -> Html Msg
wrapper gmbrd gameNumbers overlay = 
    div[class "site-wrapper"][
                h1[][text "Your gameboard"],
                br[][],
                div [class "container"] (flatten (Matrix.map createSquare gmbrd.gameMatrix)),
                br[][],
                br[][],
                button [onClick NextNumber][text "Next number"],
                showPlayedNumbers gameNumbers.playedNums,
                overlay

            ]

playAgainOverlay : Int -> Html Msg
playAgainOverlay rounds= 
            div[class "congratulations"][
                h1[][text "Congratulations!"],
                p[][text ("You won after " ++ (toString rounds) ++ " rounds!")],
                p[][text "Do you want to play again?"],
                button [onClick StartGame][text "Play again!"] 
            ]

startScreen : Html Msg
startScreen = 
    div[class "site-wrapper"][
                h1[][text "Elm Bingo!"],
                h2[][text "Instructions"],
                p[][text """Your objective is to get five in a row on your gameboard either vertically, horizontally or diagonally. 
                'Next number' button gives you the next number to check on your gameboard. 
                After you see you have the same number on your gameboard, 
                just click on the correct number on your gameboard and that square will turn green.
                The centermost square is a so-called 'free square'.
                It can be used to for 5 in a rows. 
                When you have 5 in a row in your gameboard, you have won the game."""],
                p[][text "Simple, isn't it?"],
                br[][],
                h2[][text "Do you want to start the game?"],
                br[][],
                button [onClick StartGame][text "Start game!"],
                br[][],
                br[][],
                br[][],
                p[][text "Author:"],
                p[][text "Aleksi Väisänen 2018"]
            ]

view : Model -> Html Msg
view model =
    case model of
        BeginGame ->
            startScreen

        Playing gameboard gameNums->
            wrapper gameboard gameNums (text "")

        GameOver gameboard gameNums rounds->
            wrapper gameboard gameNums (playAgainOverlay rounds)
            