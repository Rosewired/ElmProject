
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
import Random exposing (..)
-- A library for styling and displaying text.
import Text
import Time exposing (Time)
import Color exposing (..)
import Collage exposing (..)
import Html.Attributes exposing (style)
-- Animation elements
--import Svg exposing (..)
--import Svg.Attributes exposing (..)
import Char

blockSize = 15
foodRadius = 7.5
(width, height) = (550, 550)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Game
    = NewGame
    | InGame Snake Food Score
    | Lose Snake Food Score

type alias Score = Int

type Direction = Up
               | Down
               | Left
               | Right

type alias Block = { x : Float, y : Float}

pos : Float -> Float -> Block
pos = (,)

type alias Snake =
  { head : Block
  , tail : List Block
  , direction : Direction
  }

type alias Food = Maybe Block

-- define the initial state
initSnake : Snake
initSnake =
  { head = Block 28 28
  , tail = [ Block 43 28, Block 58 28, Block 73 28]
  , direction = Left
  }

init : (Game, Cmd Msg)
init = (NewGame, Cmd.none)

-- MESSAGES
type Msg
  = Tick Time
  | KeyPress Keyboard.KeyCode
  | Spawn (Float, Block)

randPos = Random.pair (Random.float 0 1) (Random.float 0 1)
generator: Random.Generator (Float, Block)
generator = Random.pair (Random.float 0 1) randPos

-- UPDATE
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case game of
    NewGame ->
      case msg of
        -- space
        KeyPress 32 ->
          (InGame initSnake Nothing 0, Cmd.none)
        _ ->
          (game, Cmd.none)

    InGame snake food score ->
      case msg of
        KeyPress key ->
          let newDirection = 
            if Char.fromCode key == 'w' && not (snake.direction == Down) then Up
            else if Char.fromCode key == 's' && not (snake.direction == Up) then Down
            else if Char.fromCode key == 'a' && not (snake.direction == Right) then Left
            else if Char.fromCode key == 'd' && not (snake.direction == Left) then Right
            else snake.direction
              newSnake = { snake | direction=newDirection }
          in (InGame newSnake food score, Cmd.none)

        Spawn (chance, (randX,randY)) ->
          if chance <= 0.1 then
            let newFood = spawnFood randX randY
            in (InGame snake newFood score, Cmd.none)
          else (game, Cmd.none)

        Tick _ ->
          let newHead = 
            if snake.direction == Up then pos snake.head.x (snake.head.y+blockSize)
            else if snake.direction == Down then pos snake.head.x (snake.head.y-blockSize)
            else if snake.direction == Left then pos (snake.head.x-blockSize) snake.head.y
            else pos (snake.head.x+blockSize) snake.head.y
              ateFood = 
                case food of 
                  Just block -> overlap newHead block
                  Nothing -> False
              newTail =
                if ateFood then snake.head::snake.tail
                else snake.head::(List.take (List.length snake.tail-1) snake.tail)
              newSnake = {snake | head=newHead, tail=newTail}
              (newFood, newScore) =
                if ateFood then (Nothing, score+1)
                else (food, score)
              gameEnd = gameOver newHead newTail
            in if gameEnd then
                (Lose snake food score, Cmd.none)
               else
                if newFood == Nothing then
                  (InGame newSnake newFood newScore, Random.generate Spawn generator)
                else
                  (InGame newSnake newFood newScore, Cmd.none)

    Lose snake food score ->
      case msg of
        -- space
        KeyPress 32 ->
          (NewGame, Cmd.none)
        _ ->
          (game, Cmd.none)

{-
updateGame : Game -> ( Game, Cmd Msg )
updateGame game =  ( game, Cmd.none )
--        |> checkIfOutOfBounds
--        |> checkIfEatenSelf
--        |> checkIfAteFruit
          |> updateSnake
--        |> updateFruit

updateSnake : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
updateSnake ( game, cmd ) =
    let
        head = game.snake.head

        newHead =
            case game.snake.direction of
                Up ->
                    { head | y = head.y + 1 }

                Down ->
                    { head | y = head.y - 1 }

                Left ->
                    { head | x = head.x - 1 }

                Right ->
                    { head | x = head.x + 1 }

        tailPositions =
                List.take (List.length game.snake.tail - 1) game.snake.tail

        tailXs =
            head.x :: List.map .x tailPositions

        tailYs =
            head.y :: List.map .y tailPositions

        newTail =
            List.map2 Block tailXs tailYs
    in
        ( { game | snake.head = newHead snake.tail = newTail }, cmd )

-}


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
          InGame snake food score ->
              let head = rect blockSize blockSize
                      |> filled white
                      |> move (snake.head.x,snake.head.y)
                  tail = snake.tail
                      |> List.map (\block -> rect blockSize blockSize
                                          |> filled yellow
                                          |> move (block.x,block.y))
                  showscore = txt (toString score)
              in case food of
                  Nothing -> showscore::head::tail
                  Just block ->
                    (circle foodRadius |> filled red |> move (block.x,block.y))::showscore::head::tail
          Lose snake food score ->
            [txt "Sorry, you lose the game, press SPACE to create a new game"]
  in collage width height (background::content)
    |> Element.toHtml


-- SUBSCRIPTIONS
subscriptions : Game -> Sub Msg
subscriptions game =
  case game of
    InGame snake food score->
      Sub.batch
        [ Keyboard.downs KeyPress
        , Time.every (Time.inMilliseconds 60) Tick
        ]
    _ ->
      Keyboard.presses KeyPress

gameOver: Block -> List Block -> Bool
gameOver newHead newTail =
  List.any ((==) newHead) newTail
  || Tuple.first newHead > (width / 2)
  || Tuple.second newHead > (width / 2)
  || Tuple.first newHead < (-width / 2)
  || Tuple.second newHead < (-width / 2)

spawnFood: Float -> Float -> Food
spawnFood randWidth randHeight =
  let x = randWidth * width - width / 2
      y = randHeight * height - height /2
  in pos x y |> Just

overlap: Block -> Block -> Bool
overlap(snakeX, snakeY) (foodX, foodY) =
  let (x,y) = (foodX-snakeX, foodY-snakeY)
      distance = sqrt(x*x+y*y)
  in distance <= (foodRadius *2)
