import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Text exposing (..)
import Window

-- MARK: constants

(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)

-- MARK: model

type alias Input =
  { space: Bool,
    delta: Time
  }

type alias Object a =
  { a |
    x: Float,
    y: Float,
    vx: Float,
    vy: Float,
    width: Float,
    height: Float
  }

type alias Bird =
  Object {}

type PipeOrigin = Top | Bottom
type alias Pipe =
  Object {
    origin: PipeOrigin
  }

type State = Waiting | Playing | Dead

type alias Game =
  {
    state: State,
    bird: Bird,
    pipes: List Pipe
  }

-- MARK: update

stepGame: Input -> Game -> Game
stepGame input game =
  let
    {space} = input
    {state, bird, pipes} = game

    state' = state
    bird' = bird
    pipes' = pipes
  in
  { game |
    state = state',
    bird = bird',
    pipes = pipes'
  }

initialState : Game
initialState =
  {
    state = Waiting,
    bird = {x = 0, y = 0, vx = 0, vy = 0, width = 15, height = 15},
    pipes = []
  }

gameState : Signal Game
gameState =
  Signal.foldp stepGame initialState input

-- MARK: view
displayObj : Object a -> Shape -> Form
displayObj obj shape =
  move (obj.x, obj.y) (filled white shape)

backgroundColor = rgb 20 20 155
whiteColor = rgb 255 255 255
txt f = leftAligned << f << monospace << Text.color whiteColor << fromString
msg = "SPACE to start"

-- display a game state

delta : Signal Time
delta =
  Signal.map inSeconds (fps 60)

-- NOTE: |> and <| are aliases for function appliaction
-- so f <| x = f x
--    x |> f = f x (to reduce parenthesis usage)
input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map2 Input
      Keyboard.space
      delta

display : (Int, Int) -> Game -> Element
display (w, h) {state, bird, pipes} =
  container w h middle <|
      collage gameWidth gameHeight
        [ filled backgroundColor (rect gameWidth gameHeight),
          displayObj bird (rect bird.height bird.width),
          toForm (if state == Playing then spacer 1 1 else txt identity msg)
            |> move (0, 40 - gameHeight / 2)
        ]

main =
  Signal.map2 display Window.dimensions gameState
