module NoIndestructible (solveProblem) where

import Control.Monad (filterM)
import Data.Function (on)
import Data.List (sortBy)

-- To implement :
-- DONE solvable (weight differences between powerset members smaller than threshold + weight of all weights (non-people things)
-- solve (knapsack problem variant): fewest "moves" inside threshold to get from a to b

data Pos = Up | Down deriving (Show, Eq)

-- | A simple datatype for a weight. It consists of an integer weight and a
-- | boolean value whether it is a person or not. It also has a
-- | position indicating whether it is up or down.
data Weight = W Bool Int Pos deriving (Show, Eq)

-- | A problem is determined by its state and a weight threshold.
data Problem = P [Weight] Int

infixr 3 .&&.

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p .&&. q = \x -> p x && q x

solveProblem = undefined

isSolvable :: Problem -> Int -> Bool
isSolvable (Problem weights threshold) =
        all (< threshold + weightBy isWeight weights) $ zipWith (-) (tail w) w
        	where
        		w = relevantWeights weights


relevantWeights w = filter ((>= weightBy (isDown .&&. isPerson) w) .&&. (<= weightBy isPerson w))
                   . map getWeight
                   $ allWeights w


getWeight :: [Weight] -> Int
getWeight = foldr (\w acc -> weight w + acc) 0

weight :: Weight -> Int
weight (W _ w _) = w

weightBy :: (Weight -> Bool) -> [Weight] -> Int
weightBy p = getWeight . filter p

-- | Predicates for weights
isUp :: Weight -> Bool
isUp (W _ _ p) = p == Up

isDown :: Weight -> Bool
isDown = not . isUp

isPerson :: Weight -> Bool
isPerson (W p _ _) = p

isWeight :: Weight -> Bool
isWeight = not . isPerson

allWeights :: [Weight] -> [[Weight]]
allWeights = sortBy (compare `on` getWeight) . filterM (const [True, False])

test :: Problem
test = P [W False 75 Up, W True 90 Up, W True 105 Up, W True 195 Up] 15
