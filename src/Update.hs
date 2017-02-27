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
updateKeyStates Nothing state = state
updateKeyStates (Just event) state = case event of
        SFEvtLostFocus -> state { paused = True }
        SFEvtKeyPressed code _ _ _ _ -> state
        _ -> state

updateGame :: GameState -> GameState
updateGame = id