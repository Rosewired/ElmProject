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
-- Animation elements
--import Svg exposing (..)
--import Svg.Attributes exposing (..)
import Char

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
    | Lose Score

type alias Score = Int

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

init : (Game, Cmd Msg)
init = (NewGame, Cmd.none)

-- MESSAGES
type Msg
  = Tick Time
  | KeyPress Keyboard.KeyCode
--  | Spawn (Float, Position)



-- UPDATE
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case game of
    NewGame ->
      case msg of
        -- space
        KeyPress 0 ->
          (InGame initSnake Nothing 0, Cmd.none)
        _ ->
          (game, Cmd.none)

    InGame snake food score ->
      case msg of
        -- shift
        KeyPress 16 ->
          (Lose score, Cmd.none)
        _ ->
          (InGame initSnake Nothing 0, Cmd.none)

    Lose score ->
      case msg of
        -- space
        KeyPress 0 ->
          (NewGame, Cmd.none)
        _ ->
          (game, Cmd.none)


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
          InGame snake food score->
            [txt "You already entered the game !!!, press SHIFT to end the game !!!"]
          Lose score ->
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
