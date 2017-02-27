module Types where

import SFML.System

type X = Double
type Y = Double
type Width = Double
type Height = Double

type Position = (X, Y)
type Size = (Width, Height)

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
                           }