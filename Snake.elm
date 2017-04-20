-- Rosewired@mac.com
module Snake exposing (..)
-- the Elm architecture
import Html
-- html style and helper functions for HTML attributes
--import Html.App
-- access of direction and key codes,
-- let application listen keyboard event
import Keyboard
-- allow us to create a grid to run the game
import Window
-- generate random place to put the food
import Random
import Time exposing (Time)
import Color exposing (..)
import Html.Attributes exposing (style)
-- Animation elements
import Svg exposing (..)
import Svg.Attributes exposing (..)

(width, height) = (500, 500)

main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- model
type alias Game =
  { snake : Snake
  , state : GameState
  , food : Block
  , eaten : Bool
  , score : Int
  }

type GameState = NewGame
               | InGame
               | Lose

type Direction = Up
               | Down
               | Left
               | Right

type alias Block = (Float, Float)

type alias Snake =
  { head : Block
  , body : List Block
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

initGame =
  { snake = initSnake
  , state = NewGame
  , food = Nothing
  , eaten = False
  , score = Int
  }

init : (Game, Cmd Msg)
init = (initGame, Cmd.none)


-- MESSAGES
type Msg
  = KeyMsg Keyboard.KeyCode

-- UPDATE
update : Msg -> Game -> (Game, Cmd Msg)
update msg ({snake, state, food, eaten, score} as game) =
  case state of
    NewGame ->
      case msg of
        -- space
        KeyPress 32 ->
          ({initSnake, InGame, Nothing, False, 0}, Cmd.none)

        _ ->
          (game, Cmd.none)
    InGame ->
      ({initSnake, InGame, Nothing, False, 0}, Cmd.none)

    Lose ->
      ({initSnake, NewGame, Nothing, False, 0}, Cmd.none)

-- VIEW
view : Game -> Html Msg
view game =
  let background = rect (toFloat width) (toFloat height) |> filled blue
  content =
    case game.state of
      NewGame ->
        [txt "press SPACE to start"]
      InGame ->
        [txt "You already enter the game !!!"]


-- SUBSCRIPTIONS
subscriptions : Game -> Sub Msg
subscriptions  game =
  case game.state of
    InGame ->
      Keyboard.presses KeyPress
    _ ->
      Sub.batch
        [ Keyboard.downs KeyMsg
        , Time.every (Time.inMilliseconds 50) Tick
        ]
