-- doing the tutorial at http://elm-lang.org/blog/making-pong
import Time exposing (..)
import Keyboard

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
near n m d =
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
stepObj t ({x, y, vx, vy} as obj) =
  { obj |
    x = x + vx * t,
    y = y + vy * t
  }

-- move a ball forward, detecting collisions with either
-- paddle and updating the position, use the `` operator
-- to perform infix `within` operations
stepBall : Time -> Ball -> Player -> Player -> Ball
stepBall t ({x, y, vx, vy} as ball) left_paddle right_paddle =
  if not (ball.x |> near 0 halfWidth)
    then { ball | x = 0, y = 0 }
  else
    stepObj t
      { ball |
        vx =
          stepV vx (ball `within` left_paddle) (ball `within` right_paddle),
        vy =
          stepV vy (y < 7 - halfHeight) (y > halfHeight - 7)
      }

-- step player forward in time making sure it doesn't
-- fly off the screen at any point in time
stepPlayer : Time -> Int -> Int -> Player -> Player
stepPlayer t direction points player =
  let player' = stepObj t { player | vy = toFloat direction * 200}
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
