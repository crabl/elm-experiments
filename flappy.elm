import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import List exposing (..)
import Random exposing (..)
import Time exposing (..)
import Text exposing (..)
import Window

-- MARK: constants

(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)
gravity = -9.81
flapVelocity = 250
pipeInterval = 80

-- MARK: model

type alias Input =
  { space: Bool,
    time: Time,
    rand: Int
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
  { state: State,
    bird: Bird,
    pipes: List Pipe
  }

-- MARK: convenience methods

near : Float -> Float -> Float -> Bool
near n d m = m >= (n - d) && m <= (n + d)

colliding a b =
  False

-- MARK: update

initialState : Game
initialState =
  { state = Waiting,
    bird = {x = 0, y = 0, vx = 0, vy = 0, width = 15, height = 15},
    pipes = []
  }

-- NOTE: |> and <| are aliases for function appliaction
-- so f <| x = f x
--    x |> f = f x (to reduce parenthesis usage)
input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map3 Input
      Keyboard.space
      delta
      rand

generateRandom : Float -> Int
generateRandom seed =
  fst <| generate (int -250 250) (initialSeed <| round seed)

rand : Signal Int
rand =
  Signal.map generateRandom (fps 60)

delta : Signal Time
delta =
  Signal.map inSeconds (fps 60)

gameState : Signal Game
gameState =
  Signal.foldp step initialState input

step: Input -> Game -> Game
step input game =
  case game.state of
    Playing -> play input game
    Waiting -> wait input game
    Dead -> die input game

play : Input -> Game -> Game
play input game =
  { game |
    state = updateState game,
    bird = updateBird input game,
    pipes = updatePipes input game
  }

updateState : Game -> State
updateState {bird, pipes} =
  if any (colliding bird) pipes
  then Dead
  else Playing

moving : Input -> Object a -> Object a
moving {time} obj =
  { obj |
    x = obj.x + obj.vx * time,
    y = obj.y + obj.vy * time
  }

flapping : Input -> Object a -> Object a
flapping {space} obj =
  { obj |
    vy = if space then flapVelocity else obj.vy
  }

falling : Input -> Object a -> Object a
falling {time} obj =
  { obj |
    vy = obj.vy + gravity * (time * 120) ^ 2
  }

updateBird : Input -> Game -> Bird
updateBird input {bird} = moving input << flapping input << falling input <| bird

updatePipes : Input -> Game -> List Pipe
updatePipes input {pipes} = pipes

wait : Input -> Game -> Game
wait ({space} as input) game =
  if space
  then play input game
  else game

die : Input -> Game -> Game
die input game = game


-- MARK: view

displayObj : Object a -> Shape -> Form
displayObj obj shape = move (obj.x, obj.y) (filled white shape)

backgroundColor = rgb 20 20 155
whiteColor = rgb 255 255 255
txt f = leftAligned << f << monospace << Text.color whiteColor << fromString
msg = "SPACE to start"

-- display a game state

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
