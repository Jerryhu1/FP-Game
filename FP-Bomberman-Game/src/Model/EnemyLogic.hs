module Model.EnemyLogic where

import Model.GameState
import Model.Player
import Model.Typeclasses.Positioned
import Model.Random

import System.Random

-- Moves randomly through positions
moveEnemy :: GameState -> Player -> GameState
moveEnemy gs enemy | (goal enemy) /= (getPos enemy) && not (checkIfPlayerCollision enemy (grid gs) )
                        = moveEnemyToPos gs enemy $ goal enemy          -- If goal is not yet met and there is no collision, keep walking
                   | (goal enemy) /= (getPos enemy) && checkIfPlayerCollision enemy (grid gs)
                        = modEnemy gs enemy (setNewGoalWhenCollision gs) -- If the goal is not met but there is a collision, set a new goal
                   | otherwise                      +
                        = modEnemy gs enemy (setNewGoal gs)              -- The goal is met and there was no collision, set new goal

setNewGoal :: GameState -> Player -> Player
setNewGoal gs e = e { goal = newGoal, playerDirection = getDirectionFromPos e newGoal  }
                  where newGoal = getPath gs e


setNewGoalWhenCollision :: GameState -> Player -> Player
setNewGoalWhenCollision gs e = e { goal = newGoal, playerDirection = getDirectionFromPos e newGoal  }
                  where newGoal = getPathWhenCollision gs e

moveEnemyToPos :: GameState -> Player -> Pos -> GameState
moveEnemyToPos gs enemy pos = modEnemy gs enemy $ checkifMovePlayer gs . changePlayerDir (getDirectionFromPos enemy pos)
                        
-- Modifies the gamestate of an enemy given a function that modifies ap layer
modEnemy :: GameState -> Player -> (Player -> Player) -> GameState
modEnemy gstate enemy f = gstate { enemies = acc enemy f (enemies gstate) }
                where acc :: Player -> (Player -> Player) -> [Player] -> [Player]
                      acc enemy f (x:[]) | enemy == x   = [f x]
                                         | otherwise    = error "Enemy doesn't exist?"
                      acc enemy f (x:xs) | enemy == x   = f x : xs
                                         | otherwise    = x : (acc enemy f xs)  
-- Sets the new goal based on the old goal
getPath :: GameState -> Player -> Pos
getPath gs p | rng == 0  = (oldX + 50, oldY)
             | rng == 1  = (oldX - 50, oldY)
             | rng == 2  = (oldX, oldY + 50)
             | otherwise  = (oldX, oldY - 50)
            where rng :: Int
                  rng = fst $ withRandom (randomR (0,3)) gs
                  (oldX,oldY) = goal p

-- Sets the new goal according to its current position
getPathWhenCollision :: GameState -> Player -> Pos
getPathWhenCollision gs p | rng == 0  = (oldX + 50, oldY)
                          | rng == 1  = (oldX - 50, oldY)
                          | rng == 2  = (oldX, oldY + 50)
                          | otherwise  = (oldX, oldY - 50)
                            where rng :: Int
                                  rng = fst $ withRandom (randomR (0,3)) gs
                                  (oldX,oldY) = playerPosition p

-- Gets the direction when moving towards a position
getDirectionFromPos :: Player -> Pos -> Direction
getDirectionFromPos p pos | getX p > fst pos = West
                          | getX p < fst pos = East
                          | getY p < snd pos = North
                          | getY p > snd pos = South
                          | otherwise        = playerDirection p
