{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( createGame
    ) where

import Types
import Update
import Render

import Paths_pong

import SFML.Utils
import SFML.Window
import SFML.Graphics

gameSize :: Size
gameSize = (640.0, 480.0)

loadResources :: IO Resources
loadResources = do
        fontPath <- getDataFileName "Vera.ttf"
        iconPath <- getDataFileName "haskell.png"
        maybeIcon <- imageFromFile iconPath
        let icon = case maybeIcon of
                    Just image -> image
                    Nothing -> (error $ "Couldn't load image: " ++ iconPath)
        font <- err $ fontFromFile fontPath
        text <- err $ createText
        setTextFont text font
        setTextCharacterSize text 16
        setTextColor text white
        setTextStringU text "Press Space To Play"
        fRect <- getGlobalBounds text
        let (width, height) = (fwidth fRect, fheight fRect)
        setPosition text $ Vec2f (320.0 - width / 2.0) (240 - height / 2.0)
        return $ Resources font text icon

createGame :: IO ()
createGame = do
        desktopMode <- getDesktopMode
        fsModes <- getFullscreenModes

        res <- loadResources

        let contextSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
        window <- createRenderWindow
                    (VideoMode
                        (round $ fst gameSize) (round $ snd gameSize) 32)
                    "Pong"
                    [SFDefaultStyle]
                    contextSettings

        setVSync window True
        Vec2u width height <- imageSize $ icon res
        pixels <- getPixels $ icon res
        setWindowIcon window (fromIntegral width) (fromIntegral height) pixels

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

        loop window res state

        destroy $ font res
        destroy $ pressSpaceToContinue res
        destroy window

loop :: RenderWindow -> Resources -> GameState -> IO ()
loop window res state = do

        event <- pollEvent window
        state' <- updateTime state >>=
                    return . updateKeyStates event >>=
                    return . updateGame

        case event of
            Just SFEvtClosed -> return ()
            _ -> renderGame window res state' >>= (\_ -> loop window res state')