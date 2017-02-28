module Render (renderGame) where

import Types

import SFML.Utils
import SFML.Graphics
import SFML.System
import GHC.Float

renderGame :: RenderWindow -> Resources -> GameState -> IO ()
renderGame window res state = do
        clearRenderWindow window black

        let (left, right) = paddles state
        renderPaddle window left
        renderPaddle window right

        let paused' = paused state
        let ball' = ball state

        if paused'
            then renderPauseScreen window res
            else renderBall window ball'


        display window

renderPaddle :: RenderWindow -> Paddle -> IO ()
renderPaddle window (Paddle (x, y) (w, h)) = do
        rect <- err createRectangleShape
        setFillColor rect white
        setSize rect $ Vec2f w h
        setPosition rect $ Vec2f x y
        drawRectangle window rect Nothing

renderBall :: RenderWindow -> Ball -> IO ()
renderBall window (Ball (x, y) r) = do
        rect <- err createRectangleShape
        setFillColor rect white
        setSize rect $ Vec2f (r * 2) (r * 2)
        setOrigin rect $ Vec2f r r
        setPosition rect $ Vec2f x y
        drawRectangle window rect Nothing

renderPauseScreen :: RenderWindow -> Resources -> IO ()
renderPauseScreen window res = do
        drawText window (pressSpaceToContinue res) Nothing