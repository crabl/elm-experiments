-- doing the tutorial at http://elm-lang.org/blog/making-pong
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Text exposing (..)
import Window

type alias Input =
  { space: Bool,
    left_paddle: Int,
    right_paddle: Int,
    delta: Time
  }

delta : Signal Time
delta =
  Signal.map inSeconds (fps 35)

-- NOTE: |> and <| are aliases for function appliaction
-- so f <| x = f x
--    x |> f = f x (to reduce parenthesis usage)
input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
      Keyboard.space
      (Signal.map .y Keyboard.wasd)
      (Signal.map .y Keyboard.arrows)
      delta

-- define the playable area and add half measures for
-- convenience later on
(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)

-- all objects in our game can be modeled as such,
-- they will all have a position and a velocity in both
-- horizontal and vertical directions
type alias GameObject a =
  { a |
    x: Float,
    y: Float,
    vx: Float,
    vy: Float
  }

type alias Ball =
  GameObject {}

-- players are GameObjects that have a score property
-- associated with them as well
type alias Player =
  GameObject { score: Int }

-- the game can be in one of two states, either Playing
-- or Paused, which we represent as a union type (enum)
type GameState = Playing | Paused

-- the Game type defines all objects that appear in our
-- game at any given point. Players, a ball, and a state
type alias Game =
  { state: GameState,
    ball: Ball,
    left_player: Player,
    right_player: Player
  }

player : Float -> Player
player x =
  { x = x,
    y = 0,
    vx = 0,
    vy = 0,
    score = 0
  }

-- defines an instance of the Game type initialized
-- with a state and properties
defaultGame : Game
defaultGame =
  { state = Paused,
    ball = { x = 0, y = 0, vx = 200, vy = 200 },
    left_player = player (20 - halfWidth),
    right_player = player (halfWidth - 20)
  }

-- collision detection system
-- are objects n and m within distance d of each other?
near : Float -> Float -> Float -> Bool
near n d m =
  m >= (n - d) && m <= (n + d)

-- is the ball contained within the paddle
within : Ball -> Player -> Bool
within ball player =
  near player.x 8 ball.x
  && near player.y 20 ball.y

-- change direction of ball velocity based on collisions
stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
  if lowerCollision then abs v
  else if upperCollision then 0 - abs v
  else v

-- here comes the tricky part!
-- step the position of the object based on its velocity
-- and a timestep, making sure not to overwrite the other
-- properties that may exist on the object
stepObj : Time -> GameObject a -> GameObject a
-- the {} as blah syntax is object destructuring with a
-- 'rest' parameter, called obj in this case, using
-- a union type as the foundation here
stepObj dt obj =
  { obj |
    x = obj.x + obj.vx * dt,
    y = obj.y + obj.vy * dt
  }

-- move a ball forward, detecting collisions with either
-- paddle and updating the position, use the `` operator
-- to perform infix `within` operations
stepBall : Time -> Ball -> Player -> Player -> Ball
stepBall dt ball left_paddle right_paddle =
  if not (near 0 halfWidth ball.x) then
    { ball | x = 0, y = 0 }
  else
    stepObj dt
      { ball |
        vx =
          stepV ball.vx (within ball left_paddle) (within ball right_paddle),
        vy =
          stepV ball.vy (ball.y < 7 - halfHeight) (ball.y > halfHeight - 7)
      }

-- step player forward in time making sure it doesn't
-- fly off the screen at any point in time
stepPlayer : Time -> Int -> Int -> Player -> Player
stepPlayer dt direction points player =
  let player' = stepObj dt { player | vy = toFloat direction * 200}
      y' = clamp (22 - halfHeight) (halfHeight - 22) player'.y
      score' = player.score + points
  in
    { player' | y = y', score = score' }

-- now that we have a ball and players, we can define a
-- step function for the entire state of the game
stepGame : Input -> Game -> Game
stepGame input game =
  let
    {space, left_paddle, right_paddle, delta} = input
    {state, ball, left_player, right_player } = game

    left_score =
      if ball.x > halfWidth then 1 else 0

    right_score =
      if ball.x < -halfWidth then 1 else 0

    state' =
      if space then Playing
      else if left_score /= right_score then Paused
      else state

    ball' =
      if state == Paused then ball
      else stepBall delta ball left_player right_player

    left_player' = stepPlayer delta left_paddle left_score left_player
    right_player' = stepPlayer delta right_paddle right_score right_player
  in
    { game |
        state = state',
        ball = ball',
        left_player = left_player',
        right_player = right_player'
    }

-- wrap it all together in a reducer!
gameState : Signal Game
gameState =
  Signal.foldp stepGame defaultGame input

-- helper values
pongGreen = rgb 60 60 100
textGreen = rgb 160 160 200
txt f = leftAligned << f << monospace << Text.color textGreen << fromString
msg = "SPACE to start, WS and &uarr;&darr; to move"

-- shared function for rendering game objects
displayObj : GameObject a -> Shape -> Form
displayObj obj shape =
  move (obj.x, obj.y) (filled white shape)

-- display a game state
display : (Int, Int) -> Game -> Element
display (w, h) {state, ball, left_player, right_player} =
  let
    scores : Element
    scores = toString left_player.score ++ "  " ++ toString right_player.score
      |> txt (Text.height 50)
  in
    container w h middle <|
      collage gameWidth gameHeight
        [ filled pongGreen (rect gameWidth gameHeight),
          displayObj ball (oval 15 15),
          displayObj left_player (rect 10 40),
          displayObj right_player (rect 10 40),
          toForm scores
            |> move (0, gameHeight / 2 - 40),
          toForm (if state == Playing then spacer 1 1 else txt identity msg)
            |> move (0, 40 - gameHeight / 2)
        ]

main =
  Signal.map2 display Window.dimensions gameState
