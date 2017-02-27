{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( createGame
    ) where

import Types
import Update
import Render

import SFML.Window
import SFML.Graphics

gameSize :: Size
gameSize = (640.0, 480.0)

createGame :: IO ()
createGame = do
        desktopMode <- getDesktopMode
        fsModes <- getFullscreenModes

        let contextSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
        window <- createRenderWindow
                    (VideoMode
                        (round $ fst gameSize) (round $ snd gameSize) 32)
                    "Pong"
                    [SFDefaultStyle]
                    contextSettings

        clock <- createClock
        time <- getElapsedTime clock

        let leftPaddle = Paddle (32.0, 32.0) (16, 48)
        let rightPaddle = Paddle (640.0 - 48.0, 32.0) (16, 48)

        let state = GameState { elapsedTime = time
                              , deltaTime = 0
                              , paddles = (leftPaddle, rightPaddle)
                              , ball = Ball (fst gameSize / 2.0, snd gameSize / 2.0) 8.0
                              , keyStates = KeyStates 0 0
                              , clock = clock
                              , score = (0, 0)
                              , paused = True
                              }

        loop window state

        destroy window

loop :: RenderWindow -> GameState -> IO ()
loop window state = do

        event <- pollEvent window
        state' <- updateTime state >>=
                    return . updateKeyStates event >>=
                    return . updateGame

        case event of
            Just SFEvtClosed -> return ()
            _ -> renderGame window state' >>= (\_ -> loop window state')