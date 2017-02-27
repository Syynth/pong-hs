module Update
    ( updateGame
    , updateTime
    , updateKeyStates
    ) where

import Types

import SFML.Window

updateTime :: GameState -> IO GameState
updateTime state = do
        let previousTime = elapsedTime state
        time <- getElapsedTime $ clock state
        return (state { elapsedTime = time, deltaTime = (asSeconds time) - (asSeconds previousTime) })

updateKeyStates :: Maybe SFEvent -> GameState -> GameState
updateKeyStates Nothing state = state
updateKeyStates (Just event) state = case event of
        SFEvtLostFocus -> state { paused = True }
        SFEvtKeyPressed code _ _ _ _ -> handleKeyPress code state
        SFEvtKeyReleased code _ _ _ _ -> handleKeyUp code state
        _ -> state

handleKeyPress :: KeyCode -> GameState -> GameState
handleKeyPress KeyUp state = state { keyStates = states' }
        where states = keyStates state
              states' = states { up = (up states) + (deltaTime state) }
handleKeyPress KeyDown state = state { keyStates = states' }
        where states = keyStates state
              states' = states { down = (down states) + (deltaTime state) }
handleKeyPress KeySpace state = state { paused = nextPaused }
        where nextPaused = not $ paused state
handleKeyPress _ state = state

handleKeyUp :: KeyCode -> GameState -> GameState
handleKeyUp KeyUp state = state { keyStates = states' }
        where states = keyStates state
              states' = states { up = 0 }
handleKeyUp KeyDown state = state { keyStates = states' }
        where states = keyStates state
              states' = states { down = 0 }
handleKeyUp _ state = state

updateGame :: GameState -> GameState
updateGame = updatePlayerPaddle . updateKeyTimes

updateKeyTimes :: GameState -> GameState
updateKeyTimes state = state { keyStates = keyStates' }
        where keyStates' = KeyStates up' down'
              upPressed = (up $ keyStates state) > 0
              downPressed = (down $ keyStates state) > 0
              up' = if upPressed then (up $ keyStates state) + (deltaTime state) else 0
              down' = if downPressed then (down $ keyStates state) + (deltaTime state) else 0

clamp mn mx = max mn . min mx

updatePlayerPaddle :: GameState -> GameState
updatePlayerPaddle state = state { paddles = newPaddles }
        where (left, right) = paddles state
              (Paddle (x, y) size) = right
              keyState = keyStates state
              dt = deltaTime state
              deltaY = (clamp 0 1 (down keyState) * dt * 300) - (clamp 0 1 (up keyState) * dt * 300)
              newPaddles = (left, movePaddle right (0, deltaY))

movePaddle :: Paddle -> Position -> Paddle
movePaddle (Paddle (x, y) size) (dx, dy) = Paddle (x + dx, clamp 32 400 (y + dy)) size