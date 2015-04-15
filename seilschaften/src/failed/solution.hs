module Main where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData, rnf)
import Control.Monad (filterM)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Function (on)
import Data.List ((\\), groupBy, intercalate, intersect, maximumBy, minimumBy, partition, sortBy, subsequences, union)
import Data.Ord (comparing)
import System.Environment (getArgs)

type ID = Int

-- | A simple datatype for a weight. It consists of an integer weight and a
-- | boolean value whether it is a person or not. It also has a
-- | position indicating whether it is up or down.
data Weight = P !Int !ID | W !Int !ID deriving Show

instance Eq Weight where
    (P _ id) == (P _ id') = id == id'
    (W _ id) == (W _ id') = id == id'
    _        == _         = False

instance NFData Weight where
    rnf (P p id) = rnf p `seq` rnf id `seq` ()
    rnf (W w id) = rnf w `seq` rnf id `seq` ()

data Tree a = Node (Tree a) a (Tree a)

-- | A problem is determined by its state and a weight threshold.
data BaseProblem = BaseProblem Int [Weight] [Weight] deriving Show

-- | The general problem also keeps track of the current number of moves and
-- | the current path.
data Problem = Problem !Int !Int (Maybe Problem) ![Weight] ![Weight] deriving Show

instance NFData Problem where
    rnf (Problem d moves prev up down) = rnf d `seq` rnf moves `seq` rnf up `seq` rnf down `seq` ()

infixr 3 .&&.

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p .&&. q = \x -> p x && q x

main :: IO ()
main = putStr . stringify . solve . parse . lines =<< readFile . head =<< getArgs

parse :: [String] -> BaseProblem
parse (x:xs) = uncurry (BaseProblem (read x))
             $ (\(a, b) -> (map fst a, map fst b))
             $ partition snd
             $ map parseWeight
             $ zip [0..]
             $ map words xs

parseWeight :: (Int, [String]) -> (Weight, Bool)
parseWeight (id, [typ, weight, pos]) = (parse' typ (read weight) id, parse'' pos)
    where
        parse' "P" = P
        parse' _ = W
        parse'' "^" = True
        parse'' _ = False
parseWeight _ = error "Wrong format"

stringify :: Maybe Problem -> String
stringify Nothing = "No solution found!"
stringify (Just p) = unlines $ stringify' p ["\nFinished state:\n"
                                            ++ display p
                                            ++ "\nMoves needed: "
                                            ++ show (numberMoves p)]
    where
        stringify' problem@(Problem _ _ Nothing _ _) xs = display problem : xs
        stringify' (Problem d moves (Just p@(Problem _ m _ u b)) up down) xs =
            stringify' p
            $ flip (:) xs
            $ "Transfer down: "
              ++ useTrick
              ++ "\n[\n"
              ++ unlines (map prettify $ intersect down u)
              ++ "]\nTransfer up:\n[\n"
              ++ unlines (map prettify $ intersect up b)
              ++ "]\nMoves (needed/total): "
              ++ show (moves - m, moves)
                  where
                      useTrick = show $ moves - m == 2


display :: Problem -> String
display (Problem d moves _ up down) = "Up:\n[\n" ++ unlines (map prettify up) ++ "]\nDown:\n[\n" ++ unlines (map prettify down) ++ "]"

prettify :: Weight -> String
prettify (P p _) = "  Person " ++ show p
prettify (W w _) = "  Weight " ++ show w

prettify' :: Weight -> String
prettify' = drop 2 . prettify



solve :: BaseProblem -> Maybe Problem
solve problem
    | null $ getPossibleSolutions problem = Nothing
    | otherwise = Just $ minimumBy (comparing numberMoves) $ getPossibleSolutions problem

getPossibleSolutions :: BaseProblem -> [Problem]
getPossibleSolutions (BaseProblem d up down) =
    filter ((==numberPeople) . length . filter isPerson . getDown)
    . simulateProblem
    $ [Problem d 0 Nothing up down]
        where
            numberPeople = length $ filter isPerson $ up `union` down

simulateProblem :: [Problem] -> [Problem]
simulateProblem [] = []
simulateProblem states = (++) states
                           $ simulateProblem
                           $ concat
                           $ parMap rdeepseq possibleMoves states

possibleMoves :: Problem -> [Problem]
possibleMoves p@(Problem d moves current up down) =
    removeBadSolutions
    $ map (\x -> makeProblem d moves (newMoves up x) current up down x)
    $ filter (((<=d) .&&. (>0)) . uncurry ((-) `on` weight))
    $ [(fromUp, toUp) | fromUp <- (++) <$> (tail $ subsequences $ filter isPerson up) <*> (subsequences $ filter isWeight up),
                        toUp <- subsequences $ weights,
                        null $ intersect fromUp toUp]
        where
            weights
                | hasPerson down = down `union` filter isWeight up
                | otherwise = filter isWeight up
            newMoves up (_, toUp)
                | null $ intersect up toUp = 1
                | otherwise = 2

removeBadSolutions :: [Problem] -> [Problem]
removeBadSolutions [] = []
removeBadSolutions xs = furthestSolutions
    where
        furthestSolutions = map (maximumBy (comparing (weight . getLowerPeople))) $ groupBy ((==) `on` numberMoves) xs
        --furthestSolution = maximumBy (comparing transferred) xs
        transferred (Problem d moves (Just (Problem _ _ _ prevUp prevDown)) up down) =
            ((+) `on` weight) (intersect up prevDown) (filter isPerson $ intersect down prevUp)
        --shortestSolution = minimumBy (comparing numberMoves) $ sortBy (flip (comparing (weight . getLowerPeople))) xs

makeProblem :: Int -> Int -> Int -> Maybe Problem -> [Weight] -> [Weight] -> ([Weight], [Weight]) -> Problem
makeProblem d moves toAdd current up down (fromUp, toUp) =
    Problem d (moves + toAdd) (Just $ Problem d moves current up down) (toUp `union` (up \\ fromUp)) (fromUp `union` (down \\ toUp))


-- | Auxiliary functions
powerset :: [Weight] -> [[Weight]]
powerset = filterM (const [True, False])

numberMoves :: Problem -> Int
numberMoves (Problem _ steps _ _ _) = steps

getDown :: Problem -> [Weight]
getDown (Problem _ _ _ _ down) = down

weight :: [Weight] -> Int
weight = foldr (\x acc -> getWeight x + acc) 0

getLowerPeople :: Problem -> [Weight]
getLowerPeople (Problem _ _ _ _ down) = filter isPerson down

getWeight :: Weight -> Int
getWeight (P p _) = p
getWeight (W w _) = w

isPerson :: Weight -> Bool
isPerson (P _ _) = True
isPerson (W _ _) = False

isWeight :: Weight -> Bool
isWeight = not . isPerson

hasPerson :: [Weight] -> Bool
hasPerson = any isPerson

test0 :: BaseProblem
test0 = BaseProblem 15 [W 75 0, P 90 1, P 105 2, P 195 3] []
