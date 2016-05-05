module Polymorphic where
import Euterpea

simple :: Integer -> Integer -> Integer -> Integer
simple x y z = x * (y + z)

applyEach :: [a -> b] -> a -> [b]
applyEach [] _ = []
applyEach (fn:fns) x = fn x : applyEach fns x

applyAll :: [a -> a] -> a -> a
applyAll [] x = x
applyAll (fn:fns) x = fn (applyAll fns x)

-- Non-recursive length function
len :: [a] -> Integer
len = let acc i _ = i + 1
      in foldl acc 0

doubleEach :: [Integer] -> [Integer]
doubleEach = map (*2)

pairAndOne :: [Integer] -> [(Integer, Integer)]
pairAndOne = let fn i = (i, i + 1)
             in map fn

addEachPair :: [(Integer, Integer)] -> [Integer]
addEachPair = map (uncurry (+))

addPairsPointwise :: [(Integer, Integer)] -> (Integer, Integer)
addPairsPointwise = let vsum (i, j) (ii, jj) = (i + ii, j + jj)
                    in foldl vsum (0, 0)

maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch [] = error "Empty list"
maxAbsPitch aps = foldl max 0 aps

minAbsPitch :: [AbsPitch] -> AbsPitch
minAbsPitch [] = error "Empty list"
minAbsPitch aps = foldl min 0 aps
