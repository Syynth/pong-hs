module Update
    ( updateGame
    , updateTime
    , updateKeyStates
    ) where

import Types

import SFML.Window

updateTime :: GameState -> IO GameState
updateTime state = do
        time <- getElapsedTime $ clock state
        return (state { elapsedTime = time })

updateKeyStates :: Maybe SFEvent -> GameState -> GameState
updateKeyStates event state = state

updateGame :: GameState -> GameState
updateGame = id