module Main where

import Control.Monad (filterM)
import Data.Function (on)
import Data.List ((\\), sortBy)
import System.Environment (getArgs)

-- To implement :
-- solvable (weight differences between powerset members smaller than threshold + weight of all weights (non-people things)
-- solve (knapsack problem variant): fewest "moves" inside threshold to get from a to b

data Pos = Up | Down deriving (Show, Eq, Read)

-- | A simple datatype for a weight. It consists of an integer weight and a
-- | boolean value whether it is a person or not. It also has a
-- | position indicating whether it is up or down.
data Weight = W Bool Int Pos deriving (Show, Eq)

-- | A problem is determined by its state and a weight threshold.
data Problem = P [Weight] Int deriving Show

infixr 3 .&&.

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p .&&. q = \x -> p x && q x

main = putStrLn . stringify . solve . parse . lines =<< readFile . head =<< getArgs

stringify [] = "No possible solution"
stringify xs = unlines $ map show xs

parse :: [String] -> Problem
parse (x:xs) = P (map (parseWeight . words) xs) (read x)

parseWeight :: [String] -> Weight
parseWeight [typ, weight, pos] = W (parse' typ) (read weight) (parse'' pos)
	where
		parse' "P" = True
		parse' _ = False
		parse'' "^" = Up
		parse'' _ = Down
parseWeight _                  = error "Wrong format"

solve :: Problem -> [[Weight]]
solve problem@(P weights threshold)
	| isSolvable = solution
	| otherwise  = []
		where
			solution = flip getSteps threshold $ relevantWeights weights
			isSolvable = all (<= threshold)
			             $ map abs
			             $ (\x -> zipWith (-) x (tail x))
			             $ map getWeightOf
			             $ map fst
			             $ filter transportedPerson
			             $ zip solution (tail solution)
				where
					transportedPerson (s1,s2) = any isPerson $ s2 \\ s1

getSteps :: [([Weight], Int)] -> Int -> [[Weight]]
getSteps [] _ = []
getSteps (state:states) threshold = fst state : (flip getSteps threshold $ dropUntilLast (valid threshold state) states)
	where
		valid t (state, w0) (newState, w1) = w1 - w0 <= t
		                                     || (all isWeight $ (newState \\ state) ++ (state \\ newState))
		                                        && filter isPerson state == filter isPerson newState

dropUntilLast :: (a -> Bool) -> [a] -> [a]
dropUntilLast p [] = []
dropUntilLast p all@(_:xs)
	| any p xs = dropUntilLast p xs
	| otherwise = all

-- | Returns all relevant weights including their total weight.
-- | Relevant are all weights including the goal and the initial position.
relevantWeights :: [Weight] -> [([Weight], Int)]
relevantWeights w = filter (pred . snd)
                    . map (\x -> (x, getWeightOf x))
                    $ allWeights w
                    	where
                    		pred = (>= getWeightBy (isDown .&&. isPerson) w)
                    		         .&&. (<= getWeightBy isPerson w)


getWeightOf :: [Weight] -> Int
getWeightOf = foldr (\w acc -> weight w + acc) 0

weight :: Weight -> Int
weight (W _ w _) = w

getWeightBy :: (Weight -> Bool) -> [Weight] -> Int
getWeightBy p = getWeightOf . filter p

-- | Predicates for weights
isUp :: Weight -> Bool
isUp (W _ _ p) = p == Up

isDown :: Weight -> Bool
isDown = not . isUp

isPerson :: Weight -> Bool
isPerson (W p _ _) = p

isWeight :: Weight -> Bool
isWeight = not . isPerson

hasPerson :: Weight -> [Weight] -> Bool
hasPerson (W _ _ p) = any (\(W t _ q) -> t && p == q)

allWeights :: [Weight] -> [[Weight]]
allWeights = sortBy (compare `on` getWeightOf) . filterM (const [True, False])

test0 :: Problem
test0 = P [W False 75 Up, W True 90 Up, W True 105 Up, W True 195 Up] 15

test1 :: Problem
test1 = P [W True 10 Down, W True 20 Down, W True 59 Down, W False 52 Down] 0

test2 :: Problem
test2 = P [W True 109 Up, W False 120 Down, W True 156 Up, W True 55 Up,
           W True 149 Up, W True 185 Up, W False 98 Down] 20

test3 :: Problem
test3 = P [W True 195 Up, W True 105 Up, W True 90 Up, W False 75 Up,
           W True 137 Up, W True 55 Up, W True 101 Down, W True 185 Up,
           W True 199 Up] 15

test4 :: Problem
test4 = P [W True 2 Up, W False 1 Down] 1

test5 :: Problem
test5 = P [W True 1 Up,W True 2 Up,W True 4 Up,W True 8 Up,W True 16 Up,
           W True 32 Up,W True 64 Up,W True 128 Up,W True 256 Up] 20

test6 = P [W True 10 Up, W False 20 Down] 5
