{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( createGame
    ) where

import SFML.Window

createGame :: IO ()
createGame = do
        desktopMode <- getDesktopMode
        fsModes <- getFullscreenModes

        let contextSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
        window <- createWindow (VideoMode 640 480 32) "Pong" [SFDefaultStyle] contextSettings

        loop window
        destroy window

loop :: Window -> IO ()
loop window = do
        event <- waitEvent window
        case event of
            Just SFEvtClosed -> return ()
            _ -> display window >>= (\_ -> loop window)

{-
import Control.Monad
import Data.Foldable (for_)
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

createGame :: IO ()
createGame = do
    SDL.initialize [SDL.InitVideo]

    SDL.HintRenderScaleQuality $= SDL.ScaleLinear
    do
        renderQuality <- SDL.get SDL.HintRenderScaleQuality
        when (renderQuality /= SDL.ScaleLinear) $
            putStrLn "Warning: Linear Texture filtering not enabled!"

    window <-
        SDL.createWindow
            "Pong"
            SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
    SDL.showWindow window

    renderer <-
        SDL.createRenderer
            window
            (-1)
            SDL.RendererConfig
                { SDL.rendererType = SDL.AcceleratedRenderer
                , SDL.rendererTargetTexture = False
                }

    SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

    let loop = do
            events <- SDL.pollEvents
            let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

            SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
            SDL.clear renderer

            SDL.present renderer

            unless quit loop

    loop


    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
-}