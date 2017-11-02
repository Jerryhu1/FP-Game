module Model.EnemyLogic where

import Model.GameState
import Model.Player
import Model.Typeclasses.Positioned
import Model.Random

import System.Random


moveEnemy :: GameState -> Player -> GameState
moveEnemy gs enemy | (goal enemy) /= (getPos enemy) = moveEnemyToPos gs enemy $ goal enemy    
                   | otherwise                       = moveEnemyToPos gs enemy $ getPath gs enemy
        
moveEnemyToPos :: GameState -> Player -> Pos -> GameState
moveEnemyToPos gs p pos | getPos p == pos     = gs
                        | otherwise           = modEnemy gs p $ checkifMovePlayer gs . changePlayerDir (getDirectionFromPos p pos)

modEnemy :: GameState -> Player -> (Player -> Player) -> GameState
modEnemy gstate enemy f = gstate { enemies = acc enemy f (enemies gstate) }
                where acc :: Player -> (Player -> Player) -> [Player] -> [Player]
                      acc enemy f (x:[]) | enemy == x   = [f x]
                                         | otherwise    = error "Enemy doesn't exist?"
                      acc enemy f (x:xs) | enemy == x   = f x : xs
                                         | otherwise    = x : (acc enemy f xs)  
                                         
getPath :: GameState -> Player -> Pos
getPath gs p | rng == 0  = (getX p + 50, getY p)
            | rng == 1  = (getX p - 50, getY p)
            | rng == 2  = (getX p, getY p + 50)
            | rng == 3  = (getX p, getY p - 50)
            | otherwise = (getX p, getY p)    
            where rng :: Int
                  rng = fst $ withRandom (randomR (0,3)) gs

                    
getDirectionFromPos :: Player -> Pos -> Direction
getDirectionFromPos p pos | getX p > fst pos = East
                        | getX p < fst pos = West
                        | getY p < snd pos = North
                        | getY p > snd pos = South
                        | otherwise        = playerDirection p
