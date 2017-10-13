-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
data Player = Player {
        health :: Int,
        name   :: String
}

instance Show Player where
        show p = "Player: " ++ name p ++ " | Health: " ++ show (health p)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                            infoToShow  :: InfoToShow 
                          , elapsedTime :: Float
                          , player      :: Player
                          }

initialState :: GameState
initialState = GameState ShowNothing 0 (Player 100 "Jerry")