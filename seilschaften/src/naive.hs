module Naive (solveProblem) where

import Control.Applicative (pure, (<*>))
import Control.Monad (filterM)
import Data.List ((\\))

data Weight  = Weight !Bool !Int deriving (Show, Eq)
data Problem = Problem [Weight] [Weight] deriving (Show, Eq)

main = putStrLn "To be implemented"

solveProblem = undefined

getAllPositions :: Problem -> [Problem]
getAllPositions (Problem lW rW) = possibleMoves lW rW ++ possibleMoves rW lW

possibleMoves :: [Weight] -> [Weight] -> [Problem]
possibleMoves lW rW = pure (transfer lW rW) <*> powerset lW <*> powerset rW

transfer :: [Weight] -> [Weight] -> [Weight] -> [Weight] -> Problem
transfer lW rW toRemove toAdd = Problem ((lW \\ toRemove) ++ toAdd) ((rW \\ toAdd) ++ toRemove)

powerset :: [Weight] -> [[Weight]]
powerset = filterM (const [True, False])
