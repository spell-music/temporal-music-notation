-- | This module defines the notion of pitch. 
module Temporal.Music.Pitch (
    Hz, Interval, c1, a1, transpose,
    -- * Pitch
    Pitch(..), fromStep, Bend, Octave, Step,
    -- * Scale
    Scale(..), fromIntervals, 
    scaleStep, scaleLength, 
    -- * PitchLike
    PitchLike(..), mapPitch,
    -- * Render  
    hz, scaleAt
)
where

import Data.Default
import qualified Data.Vector as V

-- | Cycles per second
type Hz = Double

-- | Multiplier of Hz
type Interval = Double

-- | Middle C (261.626 Hz) 
c1 :: Hz
c1 = 261.626

-- | Middle A (440 Hz) 
a1 :: Hz
a1 = 440


-- Pitch

-- | 'Pitch' denotes 'Hz' value. But it's not a double for ease of
-- performing some musical transformations, transposition, bend, 
-- inversion, changing scales. 'Pitch' can be converted to 'Hz' with 
-- function 'hz'. Pitch contains 'Scale', and point on the tone plane. 
-- The point is a triple @(bend, octave, step)@. 'Bend'
-- denotes divergens from vertices of scale grid. 'Octave' and 'Step' 
-- are integers. 
data Pitch = Pitch {
        pitchScale   :: Scale,
        pitchBend    :: Bend,
        pitchOctave  :: Octave,
        pitchStep    :: Step
    } deriving (Show, Eq)

-- | 'Bend' represents tone's diversion from scale grid. 
type Bend   = Double
type Octave = Int
type Step   = Int      

-- | Calculates position on tone plane for value of type 'Pitch'.
pitchAsDouble :: Pitch -> Double
pitchAsDouble p = pitchBend p + fromIntegral (pitchAsInt p)

-- | Calculates integer position on tone plane for value of type 'Pitch'
-- without bend bias.
pitchAsInt :: Pitch -> Int
pitchAsInt p = pitchOctave p * (scaleLength $ pitchScale p) + pitchStep p     

instance Default Pitch where
    def = Pitch def def def def

-- Scale

-- | 'Scale' defines 2D grid (octave, step) in space of 'Hz' units. 
-- 'Bend' is a level of diversion from grid vertices.
-- 1-level bend is equal to 1 step. For tones with fractional bends frequency
-- is calculated with linear interpolation by nearest values in scale.
-- Example:
--
-- > s = Scale f0 d intervals
--
-- Here scale @s@ defines 2D grid that mapps center point @(0, 0)@ to
-- frequency @f0@ 'Hz'. Value 'd' is octave interval. Octave interval
-- is divided on steps. Steps are 'Interval' s from base frequency @f0@ 
-- to desired frequency. Let's define equal temperament scale:
--
-- > eqt = Scale c1 2 $ Vector.fromList $ (map ((2 **) . (/12)) [0..11])
data Scale = Scale {
        scaleBase   :: Hz,
        scaleOctave :: Interval,
        scaleSteps  :: V.Vector Interval
    } deriving (Show, Eq)


instance Default Scale where
    def = eqt c1
        where eqt = fromIntervals 2 (map ((2 **) . (/12)) [0 .. 11]) 

-- | 'Scale' constructor.
fromIntervals :: Interval -> [Interval] -> (Hz -> Scale)
fromIntervals octave steps = \f0 -> Scale f0 octave $ V.fromList steps

-- | Scale value on doubles          
scaleAt :: Scale -> Double -> Hz
scaleAt s x = scaleAtInt s d * bendCoeff s n r 
    where (d, r) = properFraction x          
          n      = mod d $ scaleLength s


-- | 'Pitch' should be used alongside with many
-- other parameters (they can define timbre or loudness). 
-- Class 'PitchLike' provides getters and setters for
-- data types that contain value of type 'Pitch'. 
-- In "Temporal.Music.Track" module you can find many
-- functions that are defined in terms of this class. Once you
-- have chosen some note representation you can make an instance 
-- for it and use all pitch-modifiers.
class PitchLike a where
    setPitch :: Pitch -> a -> a
    getPitch :: a -> Pitch

-- | Pitch modifier.
mapPitch :: PitchLike a => (Pitch -> Pitch) -> a -> a
mapPitch f x = setPitch (f $ getPitch x) x

instance PitchLike Pitch where
    setPitch = const id
    getPitch = id


-- | Constructs 'Pitch' from some step value. 'Bend' and
-- 'Octave' are set to zeroes. 'Scale' is set to default scale
-- which is defined in 'HasScale' class.
fromStep :: Int -> Pitch
fromStep a = def{ pitchStep = a }


-- | Gives scale multiplier
scaleStep :: Scale -> Int -> Interval
scaleStep s x = (scaleOctave s ^^ o) * scaleSteps s V.! n    
    where (o, n) = divMod x $ scaleLength s


-- | Gives number of steps in one octave.
scaleLength :: Scale -> Int
scaleLength = V.length . scaleSteps


-- | Transpose cycles per second by some interval.
transpose :: Interval -> Hz -> Hz
transpose k a = k * a

---------------------------------------------------------
-- rendering

absPitch :: Pitch -> Hz
absPitch p = pitchScale p `scaleAt` pitchAsDouble p

-- | Calculates cycles per second for a pitch.
hz :: PitchLike a => a -> Hz
hz = absPitch . getPitch


-- | scale value on integers          
scaleAtInt :: Scale -> Int -> Hz
scaleAtInt s x = scaleBase s * scaleStep s x

bendCoeff :: Scale -> Int -> Double -> Hz          
bendCoeff s n x
    | abs x < 1e-6 = 1
    | x > 0        = flip loginterpCoeff x       $ getTones s n $ n + 1
    | otherwise    = flip loginterpCoeff (abs x) $ getTones s n $ n - 1
    where getTones s n1 n2 = (getTone s n1, getTone s n2)  
          getTone  s x
            | x >= 0 && x < n = scaleSteps s V.! x
            | x == n          = o
            | x == -1         = scaleSteps s V.! (n-1) / o
            | otherwise       = error $ "scaleStep: out of bounds"
            where n = scaleLength s
                  o = scaleOctave s


loginterpCoeff :: (Double, Double) -> Double -> Double
loginterpCoeff (l, r) x = (r / l) ** x


