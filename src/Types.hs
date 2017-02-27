module Types where

import SFML.System

type Position = (Float, Float)
type Size = (Float, Float)

data Paddle = Paddle Position Size
data Ball = Ball Position Float

data KeyStates = KeyStates { up :: Float
                           , down :: Float
                           }

data GameState = GameState { elapsedTime :: Time
                           , deltaTime :: Float
                           , paddles :: (Paddle, Paddle)
                           , ball :: Ball
                           , keyStates :: KeyStates
                           , clock :: Clock
                           , score :: (Int, Int)
                           , paused :: Bool
                           }