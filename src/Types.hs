module Types where

import SFML.System

type Position = (Double, Double)
type Size = (Double, Double)

data Paddle = Paddle Position Size
data Ball = Ball Position Double

data KeyStates = KeyStates { up :: Double
                           , down :: Double
                           }

data GameState = GameState { elapsedTime :: Time
                           , paddles :: (Paddle, Paddle)
                           , ball :: Ball
                           , keyStates :: KeyStates
                           , clock :: Clock
                           , score :: (Int, Int)
                           , paused :: Bool
                           }