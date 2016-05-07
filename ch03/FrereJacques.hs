module FrereJacques where
import Euterpea
import Polymorphic

rhythm :: [Dur] -> [a -> Music a]
rhythm = map note

qqqq, qqh, eee, eqq :: [Pitch -> Music Pitch]
qqqq = rhythm [qn, qn, qn, qn]
qqh = rhythm [qn, qn, hn]
eee = rhythm [en, en, en]
eqq = rhythm [en, qn, qn]

apply :: [a -> b] -> [a] -> [b]
apply = zipWith ($)

mkPhrase :: [Pitch -> Music Pitch] -> [Int] ->
  Pitch -> [Music Pitch]
mkPhrase r ins p = let t i = if i == 0 then id else trans i
                   in apply r (applyEach (map t ins) p)

frereJacques, dorMezVous, sonnezLes, matines,
  dinDanDon :: Pitch -> [Music Pitch]
frereJacques = mkPhrase qqqq [0, 2, 4, 0]
dorMezVous = mkPhrase qqh [4, 5, 7]
sonnezLes = mkPhrase eee [7, 9, 7]
matines = mkPhrase eqq [5, 4, 0]
dinDanDon = mkPhrase qqh [0, -5, 0]

mkSong :: Pitch -> [Music Pitch]
mkSong p = let phrases = [frereJacques, frereJacques,
                          dorMezVous, dorMezVous,
                          sonnezLes, matines, sonnezLes, matines,
                          dinDanDon, dinDanDon]
           in concat (applyEach phrases p)

song :: Pitch -> Music Pitch
song p = foldr (:+:) (rest 0) (mkSong p)

-- (<*>) :: [a -> b] -> [a] -> [b]
