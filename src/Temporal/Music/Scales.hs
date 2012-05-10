-- | specific scales
module Temporal.Music.Scales 
(
    -- * just scales  
    ji3, ji5, ji7,
    pyth,
    hindGb,
    hindFs,
    justBP,
    partchean, 
    luScale,
    superJust, harmonicJust,
    sruti,
    -- * Irregular scales
    eqt, eqts, eqBP, hind, 
    -- * Subscales    
    -- | extracting 5-tone scales out of 12-tone scales
    minor5, major5, 
    bluesMinor5, bluesMajor5, egyptian5,
    -- | extracting 7-tone scales out of 12-tone scales
    major, minor, 
	ionian, dorian, phrygian, lydian, 
	mixolydian, aeolian, locrian)
where

import Temporal.Music.Pitch(Hz, Interval, 
        Scale(..), scaleSize, fromIntervals)
import qualified Data.Vector as V

sliceScale :: Int -> [Int] -> Scale -> Scale 
sliceScale octaveLength ids x  
	| octaveLength == (V.length $ scaleSteps x) = 
		Scale (scaleBase x) (scaleOctave x) $
		      V.fromList $ map (scaleSteps x V.! ) ids
	| otherwise = error 
		("scale must have " ++ show octaveLength ++ 
		" tones in octave")

	
---------------------------------------------------
-- 12-tone modes


-- 5 tone

minor5       = slice12 minor5Is
major5       = slice12 major5Is
egyptian5    = slice12 egyptian5Is   
bluesMinor5  = slice12 bluesMinor5Is
bluesMajor5  = slice12 bluesMajor5Is

major5Is       = pentaIs
egyptian5Is    = rot 1 $ pentaIs
bluesMinor5Is  = rot 2 $ pentaIs
bluesMajor5Is  = rot 3 $ pentaIs
minor5Is       = rot 4 $ pentaIs

pentaIs = [2, 2, 3, 2, 2]

-- 7 tone

major, minor,
	ionian, dorian, phrygian, lydian, 
	mixolydian, aeolian, locrian :: Scale -> Scale

major = slice12 majorIs
minor = slice12 minorIs

ionian     = slice12 ionianIs
dorian     = slice12 dorianIs
phrygian   = slice12 phrygianIs
lydian     = slice12 lydianIs
mixolydian = slice12 mixolydianIs
aeolian    = slice12 aeolianIs
locrian    = slice12 locrianIs


majorIs = [2, 2, 1, 2, 2, 2, 1]
minorIs = aeolianIs

ionianIs     = rot 0 majorIs
dorianIs     = rot 1 majorIs 
phrygianIs   = rot 2 majorIs
lydianIs     = rot 3 majorIs
mixolydianIs = rot 4 majorIs
aeolianIs    = rot 5 majorIs
locrianIs    = rot 7 majorIs

slice12 :: [Int] -> (Scale -> Scale)
slice12 ids = sliceScale 12 (fromIs ids)

fromIs = (0:) . fst . foldl f ([], 0) . init
	where f (res, i) x = (res ++ [i + x], i + x)

rot :: Int -> [a] -> [a]
rot n xs = drop n xs ++ take n xs


---------------------------------------------------
-- equal temperament

-- | 12 tone equal temperament scale
eqt :: Hz -> Scale
eqt = fromIntervals 2 (map ((2 **) . (/12)) [0 .. 11])

-- | general equal temperament scale
eqts :: Hz -> Scale
eqts = res 
    where n = scaleSize $ res 0 
          res = fromIntervals 2 $ 
            (map ((2 **) . (/fromIntegral n) . fromIntegral) [0 .. n-1])

-- | hindemithean scale with mean of fs and gb for tritone
hind :: Hz -> Scale
hind = hindemitheanGen $ 0.5 * (ji5 (-1, 2, 1) + ji5 (2, -2, -1))

-- | equal Bohlen-Pierce scale
eqBP :: Hz -> Scale
eqBP = fromIntervals 3 (map ((3 **) . (/13) . fromIntegral) [0 .. 12]) 


---------------------------------------------------

-- | pythagorean scale
pyth :: Hz -> Scale
pyth = fromIntervals 2 $ map toPyth 
--  0       1        2         3        4         5      
  [(0, 0),  (-5, 3), (2, -1),  (-3, 2), (4, -2),  (-1, 1), 
--  6       7        8         9        10        11 
   (-6, 4), (1, 0),  (-4, 3),  (3, -1), (-2, 2),  (5, -2)]

toPyth :: (Int, Int) -> Interval 
toPyth (a, b) = ji3 (b, a)

-- | 3-limit basis @(2, 3\/2)@
ji3 :: (Int, Int) -> Interval
ji3 (a, b) = (2 ^^ a) * (1.5 ^^ b)

--------------------------------------------------------
-- Just intonation

-- 5-limit
--

-- | 5-limit basis @(2, 3\/2, 5\/4)@
ji5 :: (Int, Int, Int) -> Interval
ji5 (a, b, c) = (2 ^^ a) * (1.5 ^^ b) * (1.25 ^^ c)

-- | hindemithean scale with fs for tritone
hindFs :: Hz -> Scale
hindFs = hindemitheanGen $ ji5 (-1, 2, 1)

-- | hindemithean scale with gb for tritone
hindGb :: Hz -> Scale
hindGb = hindemitheanGen $ ji5 (2, -2, -1)

hindemitheanGen :: Interval -> Hz -> Scale
hindemitheanGen tritone = fromIntervals 2 $ map ji5 
--  0              1,             2,            3,              4,              5
   [(0, 0, 0),     (1, -1, -1),   (-1, 2, 0),   (0, 1, -1),     (0, 0, 1),      (1, -1, 0)]
--  6 
   ++ [tritone] ++ map ji5
--  7              8,             9,            10,               
   [(0, 1, 0),     (1, 0, -1),    (1, -1, 1),   (2, -2, 0),     (0, 1, 1)]

-- 7-limit

-- | 7-limit basis @(2, 3\/2, 5\/4, 7\/6)@
ji7 :: (Int, Int, Int, Int) -> Interval
ji7 (a, b, c, d) = (2 ^^ a) * (1.5 ^^ b) * (1.25 ^^ c) * ((7/6) ^^ d)

-- | just Bohlen-Pierce scale
justBP :: Hz -> Scale
justBP = fromIntervals 3  
-- 0        1,        2,         3,            
  [1,       27/25,    25/21,     9/7,
-- 4,       5,        6,         7 
   7/5,     75/49,    5/3,       9/5,
-- 8,       9,        10,        11
   49/25,   15/7,     7/3,       63/25,
-- 12
   25/9]


-- | Harry Partch's 43-tone scale
partchean :: Hz -> Scale
partchean = fromIntervals 2   
--  0,          1,            2,           3,           4,
   [1,          81/80,        33/32,       21/20,       16/15,   
--  5,          6,            7,           8,           9, 
    12/11,      11/10,        10/9,        9/8,         8/7,

--  10,         11,           12,          13,          14  
    7/6,        32/27,        6/5,         11/9,        5/4, 
--  15,         16,           17,          18,          19, 
    14/11,      9/7,          21/16,       4/3,         27/20,

--  20,         21,           22,          23,          24,  
    11/8,       7/5,          10/7,        16/11,       40/27, 
--  25,         26,           27,          28,          29,  
    3/2,        32/21,        14/9,        11/7,        8/5,  

--  30,         31,           32,          33,          34,  
    18/11,      5/3,          27/16,       12/7,        7/4,
--  35,         36,           37,          38,          39, 
    16/9,       9/5,          20/11,       11/6,        15/8,

--  40,         41,           42,        
    40/21,      64/33,        160/81] 

-- | Chinese Lu 12-tone scale
luScale :: Hz -> Scale
luScale = fromIntervals 2 
--  0,          1,            2,           3,           4,
   [1,          18/17,        9/8,         6/5,         54/43,   
--  5,          6,            7,           8,           9, 
    4/3,        27/19,        3/2,         27/17,       27/16,
--  10,         11,           12,          13,          14  
    9/5,        36/19] 

-- | Wendy Carlos super just 12-tone scale
superJust :: Hz -> Scale
superJust = fromIntervals 2 
--  0,          1,            2,           3,           4,
   [1,          17/16,        9/8,         6/5,         5/4,   
--  5,          6,            7,           8,           9, 
    4/3,        11/8,         3/2,         13/8,        5/3,
--  10,         11,           12,          13,          14  
    7/4,        15/8] 

-- | Wendy Carlos harmonic 12-tone scale
harmonicJust :: Hz -> Scale
harmonicJust = fromIntervals 2  
--  0,          1,            2,           3,           4,
   [1,          17/16,        9/8,         19/16,       5/4,   
--  5,          6,            7,           8,           9, 
    21/16,      11/8,         3/2,         13/8,        27/16,
--  10,         11,           12,          13,          14  
    7/4,        15/8] 


-- | Indian Sruti 22-tone scale 

sruti :: Hz -> Scale
sruti = fromIntervals 2  
--  0,          1,            2,           3,           4,
   [1,          256/243,      16/15,       10/9,        9/8,   
--  5,          6,            7,           8,           9, 
    32/27,      6/5,          5/4,         81/64,       4/3,
--  10,         11,           12,          13,          14,
    27/20,      45/32,        729/512,     3/2,         128/81,   
--  15,         16,           17,          18,          19, 
    8/5,        5/3,          27/16,       16/9,        9/5,
--  20,         21,              
    15/8,       243/128] 

