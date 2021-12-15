{-|
Module      : Project 2, COMP30020 Declarative programming
Author      : TianHao Ye, ID: 1014747, 2021 Sep.

Purpose     : Two-player logical guessing game written in Haskell.
Description : The program is about the modelling of a battleship guessing game 
in the perspective of two players, where searcher guess the three locations 
of ships on the grid, and hider will respond to searcher with feedback of guess,
the game end when searcher make a guess that exactly match the locations of ships.
In terms of the strategy uesed by searcher, the initial guess function returns 
1. an initial guess which gives least expected remaining possible targets and 
2. a game state keeping the remaining 4960 possible targets. The nextGuess 
function 1.remove inconsistent targets from the remaining possible target
2. find the best next guess from refined targets, the selectNextGuess function 
computes the expected remaining target of a guess by calculating its expected 
feedbacks to every possible remaining targets, and select the guess with least 
expected remaining targets.

note: the initial guess is calculated by function bestInitialGuess, but it does 
not run in grok environment, the code was ran in local machine and result guess 
was directly hard type into the initialGuess function. The result gives 8 best 
initial points, but only two types due to the symmetri propertiesof the board,
1. (A1, A3, H1) 2. (A2, A4, H1)
-}
module Proj2 (Location(..), toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where
import Data.List
import Data.Maybe
import Data.Char
import Data.Char(digitToInt)
import Data.Function (on)
import Data.List (sortBy)

data Location = Loc Int Int
    deriving (Eq, Show, Ord)

type GameState =  [[Location]]

-- |The 'toLocation' function convert a string location into Location data structure
-- Argument 1: 'String'.
toLocation :: String -> Maybe Location
toLocation s =
    case s of
        [] -> Nothing
        (v:[]) -> Nothing
        (v:r:[_]) -> Nothing
        (v:r:[]) -> testLocation s

-- |The 'testLocation' function test whether a location string is legal
-- Argument 1: 'String'.
testLocation :: String -> Maybe Location
testLocation (v:r:[])
    | (v `elem` ['A'..'H']) && (r `elem` ['1'..'4']) 
        = Just (Loc (alphabetToInt v) (digitToInt r))
    | otherwise = Nothing

-- |The 'alphabetToInt' function convert an alphabet to corresponding integer.
-- (e.g. A->1, B->2)
-- Argument 1: 'Char'.
alphabetToInt :: Char -> Int
alphabetToInt a = (ord a) - 64

-- |The 'intToAlphabet' function convert an inveger to corresponding alphabet.
-- (e.g. 1->A, 2->B-)
-- Argument 1: 'Int'.
intToAlphabet :: Int -> Char
intToAlphabet a = chr (a + 64)

-- |The 'fromLocation' function convert Location data structure into a string 
-- Argument 1: 'Location'.
fromLocation :: Location -> String
fromLocation (Loc v r) = (intToAlphabet v):(intToDigit r):[]

-- |The 'feedback' function gives feedback info of a guess given target
-- Argument 1: '[Location]' indicating target.
-- Argument 2: '[Location]' indicating guess.
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback t g
    = countDistanceNumber (0,0,0) minDistance
      where minDistance =  map minimum distanceList
            distanceList = guessToTargetsDistance t g

-- |The 'countDistanceNumber' function count the number of location exactly 
--- match, 1 distance away, 2 distance away.
-- Argument 1: '(Int,Int,Int)' indicating distance occurencies
-- Argument 2: '[Int]' indicating the distances
countDistanceNumber :: (Int, Int, Int) -> [Int] -> (Int, Int, Int)
countDistanceNumber (a, b, c) [] = (a, b, c)
countDistanceNumber (a, b, c) (x:xs)
    | x==0 = countDistanceNumber (a+1, b, c) xs
    | x==1 = countDistanceNumber (a, b+1, c) xs
    | x==2 = countDistanceNumber (a, b, c+1) xs
    | otherwise = countDistanceNumber (a, b, c) xs

-- |The 'guessToTargetsDistance' function compute distance between a guess 
-- to every target.
-- Argument 1: '[Location]', indicating targets, 
-- Argument 2: '[Location]', indicating guesses.
guessToTargetsDistance :: [Location] -> [Location] -> [[Int]]
guessToTargetsDistance t [] = []
guessToTargetsDistance targets (x:xs)
    = [computeDistance t x|t<-targets]:guessToTargetsDistance targets xs

-- |The 'computeDistance' function compute distance between two locations.
-- Argument 1: 'Location'.
-- Argument 2: 'Location'.
computeDistance :: Location -> Location -> Int
computeDistance (Loc v1 r1) (Loc v2 r2)= max (abs (v1-v2)) (abs (r1-r2))

-- |The 'initialGuess' function gives initial guess of searcher and remaining
-- possible targets 
initialGuess :: ([Location],GameState)
initialGuess =  (fstGuess, tars)
                where tars = combinations 3 [Loc v r | v<- [1..8], r <- [1..4]]
                      fstGuess = [(Loc 1 1), (Loc 1 3), (Loc 8 1)]
--                      fstGuess = bestInitialGuess tars

-- |The 'bestInitialGuess' function select best initial guess of searcher
-- Argument 1: 'GameState' indicates remaining possible targets
bestInitialGuess :: GameState -> [Location]
bestInitialGuess tars = selectNextGuess tars

-- |The 'combinations' function generate all n-combination of elements from a 
-- list regardless of order.
-- Argument 1: 'Int' specifies n.
-- Argument 2: '[a]' specifies the list.
combinations :: Ord a => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs@(y:ys)
    | n < 0     = []
    | otherwise = case drop (n-1) xs of
        [ ] -> []
        [_] -> [xs]
        _   -> [y:c | c <- combinations (n-1) ys] ++ combinations n ys

-- |The 'nextGuess' function gives next best guess of searcher 
-- Argument 1 : '([Location],GameState)' specifies previous guess and game state
-- Argument 2 : '(Int, Int, Int)' specifies previous guess's feedback
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (pg, gs) pf = (selectedGuess, refinedTars)
            where refinedTars = refineTargets (pg, gs) pf
                  selectedGuess = selectNextGuess refinedTars
                  
-- |The 'selectNextGuess' function select best guess from possible targets.
-- Argument 1: 'GameState'.
selectNextGuess :: GameState -> [Location]
selectNextGuess targets
    = fst (bestGuessWithValue)
    where expectedRemainings = map (\g -> (g, evalGuess g targets)) targets
          sortedRemainings = sortBy (compare `on` snd) expectedRemainings
          bestGuessWithValue = head sortedRemainings

-- |The 'evalGuess' function gives average remaining targets left after 
-- taking this guess.
-- Argument 1: '[Location]'
-- Argument 2: 'GameState'.
evalGuess :: [Location] -> GameState -> Float
evalGuess guess targets 
    = (fromIntegral (sum (map (^2) feedbackOccurrency))) / (fromIntegral len)
    where allFeedbacks = map (\target -> feedback target guess) targets
          feedbackOccurrency = freqs allFeedbacks
          len = length targets

-- |The 'freqs' function count occurencies of elements of a list.
-- Argument 1: '[(Int,Int,Int)]'.
freqs :: [(Int,Int,Int)] -> [Int]
freqs x = map length (group (sort x))

-- |The 'isConsistentTarget' function test whether the possible target consistent 
-- with previous feedback.
-- Argument 1: '[Location]' indicating previous guess, 
-- Argument 2: '(Int,Int,Int)' indicating previous feedback
-- Argument 3: '[Location]' indicating current target.
isConsistentTarget :: [Location] -> (Int,Int,Int) -> [Location] -> Bool
isConsistentTarget pg pf ct 
    = (feedback ct pg) == pf

-- |The 'refineTargets' function eliminate inconsistent possible targets.
-- Argument 1: '([Location],GameState)' indicating previous guess and poosible targets
-- Argument 2: '(Int,Int,Int)' indicating previous feedback.
refineTargets :: ([Location],GameState) -> (Int,Int,Int) -> GameState
refineTargets (pg, gameState) pf 
    = filter (\t -> (isConsistentTarget pg pf t) == True) gameState
    