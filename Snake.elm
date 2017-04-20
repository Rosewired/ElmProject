-- the Elm architecture
import Html
-- html style and helper functions for HTML attributes
--import Html.App
-- access of direction and key codes
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

main =
  App.program
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

type alias GameState = NewGame
                     | InGame
                     | Lose

type Direction = Up
               | Down
               | Left
               | Right

-- key presses
type Keys = NoKey
          | UpKey
          | DownKey
          | LeftKey
          | RightKey

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

init : Game
init =
  { snake = initSnake
  , state = NewGame
  , food = Nothing
  , eaten : False
  , score : Int
  }

-- define subscriptions
