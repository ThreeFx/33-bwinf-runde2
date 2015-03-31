module Naive (solveProblem) where

import Control.Applicative (pure, (<*>))
import Control.Monad (filterM)
import Data.List ((\\))
import Data.Maybe (catMaybes)

data Weight  = Weight !Bool !Int deriving (Show, Eq)
data Problem = Problem [Weight] [Weight] !Int deriving (Show, Eq)

solveProblem = undefined

getAllPositions :: Problem -> [Problem]
getAllPositions (Problem u d t) = possibleMoves u d t

possibleMoves :: [Weight] -> [Weight] -> [Problem]
possibleMoves l r t = catMaybes $ pure (transfer t l r) <*> powerset l <*> powerset r

transfer :: Int -> [Weight] -> [Weight] -> [Weight] -> [Weight] -> Maybe Problem
transfer t l r li ri
  | abs (weight li - weight ri) <= d || all isWeight li && all isWeight ri =
          Just $ Problem ((l \\ li) ++ ri) ((r \\ ri) ++ li)
  | otherwise = Nothing

weight :: [Weight] -> Int
weight = foldMap 

powerset :: [Weight] -> [[Weight]]
powerset = filterM (const [True, False])

test :: Problem
test = Problem [Weight False 75, Weight True 90, Weight True 105, Weight True 195] []

test1 :: Problem
test1 = Problem [Weight False 75] []

test2 :: Problem
test2 = Problem [Weight False 75] [Weight True 90]
