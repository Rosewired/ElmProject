-- Rosewired@mac.com
module Snake exposing (..)
-- the Elm architecture
import Html exposing (..)
import Html.Events exposing (..)
import Element exposing (..)
-- html style and helper functions for HTML attributes
--import Html.App
-- access of direction and key codes,
-- let application listen keyboard event
import Keyboard
-- allow us to create a grid to run the game
import Window
-- generate random place to put the food
import Random
import Text
import Time exposing (Time)
import Color exposing (..)
import Collage exposing (..)
import Html.Attributes exposing (style)
-- Animation elements
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Char

(width, height) = (500, 500)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- model
--type alias Game =
  --{ snake : Snake
  --, state : GameState
  --, food : Block
  --, eaten : Bool
  --}

type Game
  = NewGame
  | InGame Snake Food Score

type alias Score = Int

--type GameState = NewGame
               --| InGame
               --| Lose

type Direction = Up
               | Down
               | Left
               | Right

type alias Block = (Float, Float)

type alias Snake =
  { head : Block
  , tail : List Block
  , direction : Direction
  }

type alias Food = Maybe Block

-- define the initial state
initSnake : Snake
initSnake =
  { head = (28, 28)
  , tail = [(29, 28), (30, 28)]
  , direction = Left
  }

--initGame =
  --{ snake = initSnake
  --, state = NewGame
  --, food = Nothing
  --, eaten = False
  --}

init : (Game, Cmd Msg)
init = (NewGame, Cmd.none)


-- MESSAGES
type Msg
  = Tick Time
  | KeyPress Keyboard.KeyCode
  | Spawn (Float, Position)


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
    InGame snake food score->
      (InGame initSnake Nothing 0, Cmd.none)

    --Lose ->
      --(NewGame initSnake Nothing False 0, Cmd.none)

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
            [txt "press SPACE to start"]
          InGame snake food score->
            [txt "You already enter the game !!!"]
  in collage width height (background::content)
    |> Element.toHtml


-- SUBSCRIPTIONS
subscriptions : Game -> Sub Msg
subscriptions  game =
  case game of
    InGame snake food score->
      Keyboard.presses KeyPress
    _ ->
      Sub.batch
        [ Keyboard.downs KeyPress
        , Time.every (Time.inMilliseconds 50) Tick
        ]
