-- Rosewired@mac.com

module Snake exposing (..)
-- the Elm architecture
import Html exposing (..)
import Html.Events exposing (..)
import Element exposing (..)
-- access of direction and key codes,
-- let application listen keyboard event
import Keyboard
-- allow us to create a grid to run the game
import Window
-- generate random place to put the food
import Random
-- A library for styling and displaying text.
import Text
import Time exposing (Time)
import Color exposing (..)
import Collage exposing (..)
import Html.Attributes exposing (style)
import Char

blockSize = 15
foodRadius = 7.5
(width, height) = (500, 500)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias GameState = { snake : Snake,
                         food : Food,
                         score: Score,
                         isDead : Bool
                       }

type alias Score = Int

type Game
    = NewGame
    | InGame GameState
    | Lose Score



type Direction = Up
               | Down
               | Left
               | Right

type alias Block = { x : Float
                   , y : Float
                   }

type alias Snake =
  { body : List Block
  , direction : Direction
  }

type alias Food = Maybe Block

-- define the initial state
initSnake : Snake
initSnake =
  { body = [ Block 0 0, Block blockSize 0, Block (2*blockSize) 0, Block (3*blockSize) 0]
  , direction = Left
  }

initFood : Food
initFood = Just (Block -100 28)

init : (Game, Cmd Msg)
init = (NewGame, Cmd.none)

initScore : Score
initScore = 0

initGameState : GameState
initGameState = {snake = initSnake,
                 food = initFood,
                 score = initScore,
                 isDead = False}
-- MESSAGES
type Msg
  = Tick Time
  | KeyPress Keyboard.KeyCode


-- UPDATE
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case game of
    NewGame ->
      case msg of
        -- space
        KeyPress 32 ->
          (InGame initGameState, Cmd.none)
        _ ->
          (game, Cmd.none)

    InGame gameState ->
      case msg of
        -- shift
        KeyPress 16 ->
          (Lose gameState.score, Cmd.none)

        KeyPress keyCode ->
          case gameState of
            snake ->
              (InGame (updateDirection keyCode gameState), Cmd.none)

        Tick time ->
          let newState = updateGame gameState
          in
            case gameState.isDead of
              True ->
                (Lose gameState.score, Cmd.none)
              _ ->
                (InGame newState, Cmd.none)

--        _ ->
--          (InGame initGameState, Cmd.none)

    Lose score ->
      case msg of
        -- space
        KeyPress 32 ->
          (NewGame, Cmd.none)
        _ ->
          (game, Cmd.none)

getSnakeHead : Snake -> Block
getSnakeHead snake =
  Maybe.withDefault (Block 0 0)
    (List.head snake.body)

updateDirection : Keyboard.KeyCode -> GameState -> GameState
updateDirection keyCode gameState =
      let
        newSnake = getNewDirection keyCode gameState.snake
      in
        { gameState | snake = newSnake}


getNewDirection : Keyboard.KeyCode -> Snake -> Snake
getNewDirection key snake =
    -- left arrow
    if (key == 37 && snake.direction /= Right )then
      {snake | direction = Left}
    -- right arrow
    else if key == 39 && snake.direction /= Left then
      {snake | direction = Right}
    -- up arrow
    else if key == 38 && snake.direction /= Down then
      {snake | direction = Up}
    -- down arrow
    else if key == 40 && snake.direction /= Up then
      {snake | direction = Down}
    else
      let
      direction = snake.direction
        in
        {snake | direction = direction}

updateGame : GameState -> GameState
updateGame  gameState = gameState
          |> checkIfOutOfBounds
--          |> checkIfEatenSelf
--        |> checkIfAteFruit
          |> updateSnake
--        |> updateFruit

checkIfOutOfBounds : GameState -> GameState
checkIfOutOfBounds gameState =
  let snakeHead = getSnakeHead gameState.snake
      isDead = ((snakeHead.x <= -width/2 && gameState.snake.direction == Left)
      || (snakeHead.y >= height/2 && gameState.snake.direction == Up)
      || (snakeHead.x >= width/2 && gameState.snake.direction == Right)
      || (snakeHead.y <= -height/2 && gameState.snake.direction == Down))
  in
    {gameState | isDead = isDead }




updateSnake : GameState -> GameState
updateSnake gameState =
    let
        snakeHead = getSnakeHead gameState.snake

        newHead =
            case gameState.snake.direction of
                Up ->
                    { snakeHead | y = snakeHead.y + blockSize }

                Down ->
                    { snakeHead | y = snakeHead.y - blockSize }

                Left ->
                    { snakeHead | x = snakeHead.x - blockSize }

                Right ->
                    { snakeHead | x = snakeHead.x + blockSize }

        newTail = gameState.snake.body
                |> List.reverse
                |> List.drop 1
                |> List.reverse

        newBody = newHead :: newTail
        snake = gameState.snake

    in
      { gameState | snake = { snake | body = newBody }}


txt : String -> Form
txt msg =
  msg
  |> Text.fromString
  |> Text.color white
  |> Text.monospace
  |> Element.centered
  |> Collage.toForm

-- VIEW
view : Game -> Html Msg
view game =
  let background = Collage.rect (toFloat width) (toFloat height) |> filled blue
      content =
        case game of
          NewGame ->
            [txt "press SPACE to start!"]
          InGame gameState ->
              let snakeHead = getSnakeHead gameState.snake
                  head = rect blockSize blockSize
                      |> filled white
                      |> move (snakeHead.x,snakeHead.y)
                  tail = gameState.snake.body
                      |> List.drop 1
                      |> List.map (\block -> rect blockSize blockSize
                                          |> filled yellow
                                          |> move (block.x,block.y))

                  showscore = txt (toString gameState.score)
              in case gameState.food of
                  Nothing -> showscore::head::tail
                  Just block ->
                    (circle foodRadius
                    |> filled red
                    |> move (block.x,block.y))::showscore::head::tail
          Lose score ->
            [txt "Sorry, you lose the game, press SPACE to create a new game"]
  in collage width height (background::content)
    |> Element.toHtml


-- SUBSCRIPTIONS
subscriptions : Game -> Sub Msg
subscriptions game =
  case game of
    InGame gameState ->
      Sub.batch
        [ Keyboard.downs KeyPress
        , Time.every (Time.inMilliseconds 60) Tick
        ]
    _ ->
      Keyboard.presses KeyPress
