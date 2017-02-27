module Render (renderGame) where

import Types

import SFML.Graphics
import SFML.System
import GHC.Float

renderGame :: RenderWindow -> GameState -> IO ()
renderGame window state = do
        clearRenderWindow window black

        let (left, right) = paddles state
        renderPaddle window left
        renderPaddle window right

        let ball' = ball state
        renderBall window ball'

        display window

renderPaddle :: RenderWindow -> Paddle -> IO ()
renderPaddle window (Paddle (x, y) (w, h)) = do
        either <- createRectangleShape
        case either of
            Left _ -> return ()
            Right rect -> do
                    setFillColor rect white
                    setSize rect $ Vec2f w h
                    setPosition rect $ Vec2f x y
                    drawRectangle window rect Nothing

renderBall :: RenderWindow -> Ball -> IO ()
renderBall window (Ball (x, y) r) = do
        either <- createCircleShape
        case either of
            Left _ -> return ()
            Right circle -> do
                    setFillColor circle white
                    setRadius circle r
                    setOrigin circle $ Vec2f r r
                    setPosition circle $ Vec2f x y
                    drawCircle window circle Nothing