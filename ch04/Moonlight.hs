module Moonlight where
import Euterpea

timesM     :: Int -> Music a -> Music a
timesM 0 _ = rest 0
timesM n m = m :+: timesM (n - 1) m

addDur      :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = let f n = n d
              in line (map f ns)

trans'     :: Int -> (Dur -> Music Pitch) -> (Dur -> Music Pitch)
trans' i n = let (Prim (Note _ p)) = n en
                 fn d              = note d (trans i p)
             in  fn

-- Arpeggios
arpeggio :: (Dur -> Music a, Dur -> Music a, Dur -> Music a) -> Music a
arpeggio (n1, n2, n3) = tempo (3/2) (addDur en [n1, n2, n3])

aGsCsE  = arpeggio (gs 3, cs 4, e 4)
aACsE   = arpeggio (a 3, cs 4, e 4)
aADFs   = arpeggio (a 3, d 4, fs 4)
aGsBsFs = arpeggio (gs 3, bs 3, fs 4)
aGsCsDs  = arpeggio (gs 3, cs 4, ds 4)
aFsBsDs  = arpeggio (fs 3, bs 3, ds 4)

aEGsCs   = arpeggio (e 3, gs 3, cs 4)
aGsDsFs  = arpeggio (gs 3, ds 4, fs 4)

arpeggios1t4 = timesM 8 aGsCsE :+: timesM 2 aACsE :+: timesM 2 aADFs :+:
               aGsBsFs :+: aGsCsE :+: aGsCsDs :+: aFsBsDs
arpeggios5t6 = aEGsCs :+: timesM 3 aGsCsE :+: timesM 4 aGsDsFs

-- Chords
mkChord      :: Dur -> [Dur -> Music a] -> Music a
mkChord d ns = let f n = n d
               in chord (map f ns)

cCsCs   = mkChord wn [cs 2, cs 3]
cBGs    = mkChord wn [b 1, b 2]
cAA     = mkChord hn [a 1, a 2]
cFsFs   = mkChord hn [fs 1, fs 2]
cGsGs   = mkChord hn [gs 1, gs 2]

cCsGsCs = mkChord wn [cs 2, gs 2, cs 3]
cBsGsBs = mkChord wn [bs 1, gs 2, bs 2]

chords1t4 = cCsCs :+: cBGs :+: cAA :+: cFsFs :+: cGsGs :+: cGsGs
chords5t6 = cCsGsCs :+: cBsGsBs

-- Voice
voice5t6 = rest hn :+: rest qn :+: gs 4 den :+: gs 4 sn :+:
           gs 4 dhn :+: gs 4 den :+: gs 4 sn

-- Song
mkSong parts = let t = wn * (65 / 120)
               in tempo t (chord parts)

song5t6 = mkSong [arpeggios5t6, voice5t6, chords5t6]
song = mkSong [arpeggios1t4 :+: arpeggios5t6,
               chords1t4 :+: chords5t6]
