module Types where

import SFML.System
import SFML.Graphics

instance Show Clock where
    show _ = "Clock"

type Position = (Float, Float)
type Size = (Float, Float)

data Paddle = Paddle Position Size deriving (Show)
data Ball = Ball Position Float deriving (Show)

data KeyStates = KeyStates { up :: Float
                           , down :: Float
                           } deriving (Show)

data GameState = GameState { elapsedTime :: Time
                           , deltaTime :: Float
                           , paddles :: (Paddle, Paddle)
                           , ball :: Ball
                           , keyStates :: KeyStates
                           , clock :: Clock
                           , score :: (Int, Int)
                           , paused :: Bool
                           } deriving (Show)

data Resources = Resources { font :: Font
                           , pressSpaceToContinue :: Text
                           , icon :: Image
                           }