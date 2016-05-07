module SimpleMusic where
import Euterpea

t251 :: Music Pitch
t251 = let dMinor = minorChord (D, 4) wn
           gMajor = majorChord (G, 4) wn
           cMajor = majorChord (C, 4) bn
       in line [dMinor, gMajor, cMajor]

-- define a chord as transpositions of a pitch
myChordDef :: [Int] -> Pitch -> Dur -> Music Pitch
myChordDef intervals p dr =
  let fn i = note dr (trans i p)
  in chord (map fn intervals)

majorChord :: Pitch -> Dur -> Music Pitch
majorChord = myChordDef [0, 4, 7]

minorChord :: Pitch -> Dur -> Music Pitch
minorChord = myChordDef [0, 3, 7]

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = let ii = minorChord (trans 2 p) d
                     v  = majorChord (trans 7 p) d
                     i  = majorChord p  (2 * d)
                 in line [ii, v, i]
