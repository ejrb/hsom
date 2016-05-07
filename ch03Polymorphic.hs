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

chromScale :: Pitch -> Pitch -> [Pitch]
chromScale p1 p2 | absPitch p1 == absPitch p2  = [p1]
                 | absPitch p1 < absPitch p2   = p1 : chromScale (trans 1 p1) p2
                 | otherwise                   = reverse (chromScale p2 p1)

chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 = foldr ((:+:) . note qn) (rest 0) (chromScale p1 p2)

mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p [] = note en p
mkScale p (int:ints) = note en p :+: mkScale (trans int p) ints

data MajorScale = Ionian | Dorian | Phrygian | Lydian | Mixolydian |
                  Aeolian | Locrian

intervallicScale :: MajorScale -> [Int]
intervallicScale ms = case ms of
  Ionian     -> [2, 2, 1, 2, 2, 2, 1]
  Dorian     -> [2, 1, 2, 2, 2, 1, 2]
  Phrygian   -> [1, 3, 1, 2, 1, 2, 2]
  Lydian     -> [2, 2, 2, 1, 2, 2, 1]
  Mixolydian -> [2, 2, 1, 2, 2, 1, 2]
  Aeolian    -> [2, 1, 2, 2, 1, 2, 2]
  Locrian    -> [1, 2, 2, 1, 2, 2, 2]

mkMajorScaleUp :: Pitch -> MajorScale -> Music Pitch
mkMajorScaleUp p ms = mkScale p (init (intervallicScale ms))

mkMajorScaleDown :: Pitch -> MajorScale -> Music Pitch
mkMajorScaleDown p ms = let intervals = init . reverse . intervallicScale
                        in  mkScale p (map negate (intervals ms))

upDownMajorScale :: MajorScale -> Pitch -> Music Pitch
upDownMajorScale ms p@(pc, o) = mkMajorScaleUp p ms :+:
                                mkMajorScaleDown (pc, o + 1) ms

allMajorScales :: Pitch -> Music Pitch
allMajorScales p =
  let scales = map upDownMajorScale [Ionian, Dorian, Phrygian, Lydian,
                                     Mixolydian, Aeolian, Locrian]
      variants = applyEach scales p
  in foldl (:+:) (rest 0) variants :+: note wn p
