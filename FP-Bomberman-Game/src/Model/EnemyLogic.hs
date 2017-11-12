module Model.EnemyLogic where

import Model.GameState
import Model.Player
import Model.Typeclasses.Positioned

import System.Random

-- Moves randomly through positions
moveEnemy :: GameState -> Player -> GameState
moveEnemy gs enemy | state enemy == Dying = updateGs gs
                   | goal enemy /= getPos enemy && not (checkCollisionSurr enemy (grid gs) )
                        = updateGs $ moveEnemyToPos gs enemy $ goal enemy          -- If goal is not yet met and there is no collision, keep walking
                   | goal enemy /= getPos enemy && checkCollisionSurr enemy (grid gs)
                        = updateGs $ modEnemy gs enemy (setNewGoalWhenCollision gs) -- If the goal is not met but there is a collision, set a new goal
                   | otherwise
                        = updateGs $ modEnemy gs enemy (setNewGoal gs)              -- The goal is met and there was no collision, set new goal
                    where updateGs gs' = snd $ withRandom next gs'

-- Sets a new goal for a given player / enemy
setNewGoal :: GameState -> Player -> Player
setNewGoal gs e = e { goal = newGoal, playerDirection = getDirectionFromPos e newGoal  }
                  where newGoal = getPath gs e

-- If there is a collision set a goal based on current position
setNewGoalWhenCollision :: GameState -> Player -> Player
setNewGoalWhenCollision gs e = e { goal = newGoal, playerDirection = getDirectionFromPos e newGoal  }
                  where newGoal = getPathWhenCollision gs e

-- Move the player/enemy to a position and change it's direction
moveEnemyToPos :: GameState -> Player -> Pos -> GameState
moveEnemyToPos gs enemy pos = modEnemy gs enemy $ changePlayerDir gs (getDirectionFromPos enemy pos)
                        
-- Modifies the gamestate of an enemy given a function that modifies ap layer
modEnemy :: GameState -> Player -> (Player -> Player) -> GameState
modEnemy gstate enemy f = gstate { enemies = acc enemy f (enemies gstate) }
                where acc :: Player -> (Player -> Player) -> [Player] -> [Player]
                      acc enemy f [x] | enemy == x   = [f x]
                                         | otherwise    = error "Enemy doesn't exist?"
                      acc enemy f (x:xs) | enemy == x   = f x : xs
                                         | otherwise    = x : acc enemy f xs

-- Sets the new goal based on the old goal
getPath :: GameState -> Player -> Pos
getPath gs p | rng == 0  = (oldX + 50, oldY)
             | rng == 1  = (oldX - 50, oldY)
             | rng == 2  = (oldX, oldY + 50)
             | otherwise  = (oldX, oldY - 50)
            where rng :: Int
                  rng = fst $ genNumberByRange gs (0,3)
                  (oldX,oldY) = goal p

-- Sets the new goal according to its current position
getPathWhenCollision :: GameState -> Player -> Pos
getPathWhenCollision gs p | rng == 0  = (oldX + 50, oldY)
                          | rng == 1  = (oldX - 50, oldY)
                          | rng == 2  = (oldX, oldY + 50)
                          | otherwise  = (oldX, oldY - 50)
                            where rng :: Int
                                  rng = fst $ genNumberByRange gs (0,3)
                                  (oldX,oldY) = playerPosition p

-- Gets the direction when moving towards a position
getDirectionFromPos :: Player -> Pos -> Direction
getDirectionFromPos p pos | x > fst pos = West
                          | x < fst pos = East
                          | y < snd pos = North
                          | y > snd pos = South
                          | otherwise        = playerDirection p
                          where (x,y) = getPos p
