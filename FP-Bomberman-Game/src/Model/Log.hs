module Model.Log where

import Model.GameState
import Control.Monad
import Data.Maybe
import Text.Read

readCurrentHighscore :: IO String
readCurrentHighscore = readFile "res/score.txt"


printCollision :: GameState -> String
printCollision gs = show $ checkCollisionSurr (player gs) (grid gs)

writeNewHighScore :: Int -> IO ()
writeNewHighScore score =
                     do
                        oldScore <- readCurrentHighscore                  
                        let intOldScore = readMaybe oldScore :: Maybe Int
                        if isJust intOldScore then when (score > fromJust intOldScore)$ writeFile "res/score.txt" $ show score 
                        else writeFile "res/score.txt" (show score)

