module TransMaker where 

import System.Time
import Data.Maybe
import Data.List
import Text.Printf

width :: Num a => a
width = 1024
height :: Num a => a
height = 768 

fs_name = 60
fs_name_big = 180
fs_word = 200


-- 1 für alles gleichzeitig
-- 0.5 für halbe zeit pro zeichen
delayLetters = 0.4

--                    Buchstabe x y fontsize flip-scale
data Coord = Coord { cX :: Double, cY :: Double, cS :: Double, cF :: Double } deriving (Eq, Ord) 

instance Show Coord where
	show (Coord x y s f) = printf "(%.0f,%.0f,%.0f,%.0f)" x y s f 

type Trans = Double -> LetterLayout
type LetterLayout = [ (Char, Coord ) ]
type LetterState = ( [Trans], [Trans] )
type TransState = (ClockTime, Trans)

type TransListHead = LetterLayout -> [Trans]
type LayoutMod = LetterLayout -> LetterLayout


{- For Parser -}

frame :: String -> String -> TransListHead -> [Trans]
frame w1 w2 trans = trans $  baseLayout w1 w2

start :: LayoutMod -> TransListHead
start mod base = [const (mod base)]

stdTrans :: LayoutMod -> TransListHead -> TransListHead
stdTrans next before base = before base ++ [morph prev (next base)] 
  where prev = last (before base) 1

flipTrans :: Char -> Char ->  TransListHead -> TransListHead
flipTrans c1 c2 before base = before base ++ [there, there . (1-) ] 
  where there d | d <  0.5 = untouched1 ++ [(c1, c {cF = 1-2*d })]     ++ untouched2
                | d >= 0.5 = untouched1 ++ [(c2, c {cF = 2*(d-0.5) })] ++ untouched2
	(untouched1,(_,c):untouched2) = span (\(c,_) -> c /= c1) prev
  	prev = last (before base) 1

fadeTrans :: TransListHead -> TransListHead
fadeTrans before base = before base ++ [\d ->  map (\(l,c) -> (l,c {cF = (1-d)})) prev]
  where prev = last (before base) 1

moveMiddle :: String -> LayoutMod
moveMiddle word base = base `moveLetters` midline
  where midline = putOnLine midlinepoint word
   	midlinepoint = Coord undefined 400 fs_word 1

moveTwoLines :: String -> String -> LayoutMod
moveTwoLines w1 w2 base = base `moveLetters` twolines
  where twolines = putOnLine line1 w1 ++ putOnLine line2 w2 
  	line1 = Coord undefined 200 fs_name_big 1
	line2 = Coord undefined 400 fs_name_big 1

moveThreeLines :: String -> String -> String -> LayoutMod
moveThreeLines w1 w2 w3  base = base `moveLetters` lines
  where lines = putOnLine line1 w1 ++ putOnLine line2 w2  ++ putOnLine line3 w3
  	line1 = Coord undefined 100 fs_name_big 1
	line2 = Coord undefined 350 fs_name_big 1
	line3 = Coord undefined 600 fs_name_big 1


moveHidden :: LayoutMod
moveHidden base = map (\(c,_) -> (c, Coord (-10) (-10) 10 1)) $ base


{- Layout Generation -}

baseLayout :: String -> String -> LetterLayout
baseLayout w1 w2 =  putOnLine line1 w1 ++ putOnLine line2 w2 
  where line1 = Coord undefined 30 fs_name 1
	line2 = Coord undefined 700 fs_name 1

moveLetters :: LetterLayout -> LetterLayout -> LetterLayout
moveLetters from to = newlayout 
  where ([], newlayout) = mapAccumL movePoint to from
        movePoint todo (l,c) = case lookup l todo of
  				Nothing   -> (todo, (l,c)) 
				Just c'   -> (delete (l,c') todo, (l,c'))

morph :: LetterLayout -> LetterLayout -> Trans
{-- Haskell Bug, I guess --}
--morph config1 config2 10 = config2 
morph config1 config2 d = config2  `moveLetters` unsorted 
  where	unsorted = unch1 ++ zipWith3 (\n -> middle (delay n d)) [0..] ch1 ch2
        (unch,ch) = partition (uncurry (==)) $ zip config1 config2
  	(unch1,_) = unzip unch
	(ch1,ch2) = unzip ch 
        l = fromIntegral $ length ch1 
	delay n d = max (min ((d-st)/delayLetters) 1) 0 
	  where st  = n * (1-delayLetters)/l 

{- Utils -}
putOnLine template word =  spaceout width (\c x -> (c, template {cX = x})) word

spaceout width f list = map (\(c,n) -> f c ((0.5 + n) * d) ) $ zip list [0..]
  where	d = width / fromIntegral (length list)

middle d' (l1,c1) (l2,c2) = (l,mc)
  where l = if l1 == l2 then l1 else error $ "Not Same "++[l1]++","++[l2]
  	mc = Coord { cX = (1-d) * cX c1 + d * cX c2,
                     cY = (1-d) * cY c1 + d * cY c2,
                     cS = (1-d) * cS c1 + d * cS c2,
		     cF = 1}
	d = if d'<0 || d'>1 then error "OOB" else 0.5*(1-cos(d' * pi))

