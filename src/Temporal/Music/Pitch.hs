{-# LANGUAGE 
       TypeFamilies, 
       FlexibleContexts #-}

-- | This module defines the notion of pitch. 
module Temporal.Music.Pitch (
    Hz, Interval, c1, a1, transpose,
    -- * Pitch
    Pitch(..), fromStep, Bend, Octave, 
    HasScale(..),
    -- * Scale
    Scale(..), fromIntervals, 
    scaleStep, scaleSize, 
    -- * PitchLike
    PitchLike(..), mapPitch,
    -- * Render  
    hz, pitchAsDouble, scaleAt
)
where

import Data.Finite
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
-- function 'hz'. Pitch contains 'Scale', and coordinates. 
-- Pitch coordinate is triple @(bend, octave, step)@. 'Bend'
-- denotes divergens from vertices of scale grid. 'Octave' is
-- an integer. Step is some 'Finite' value. 
--
-- Step is not just
-- an integer for ease of automatic 'Scale' assigning. There is
-- a class 'HasScale'. You can define your oun type for step and
-- construct pitch with 'fromStep' function. 
data Pitch a = Pitch {
        pitchScale   :: Scale,
        pitchBend    :: Bend,
        pitchOctave  :: Octave,
        pitchStep    :: Mod a
    } deriving (Show, Eq)


-- | 'Bend' represents tone's diversion from scale grid. 
type Bend    = Double
type Octave  = Int

-- | Calculates coordinates for value of type 'Pitch'.
pitchAsDouble :: Finite a => Pitch a -> Double
pitchAsDouble p = b + fromIntegral n
        where n = fromEnum o * domLength s + fromEnum s         
              s = pitchStep p
              o = pitchOctave p  
              b = pitchBend p

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

-- | 'Scale' constructor.
fromIntervals ::  
        Interval -> [Interval]
    -> (Hz -> Scale)
fromIntervals octave steps = \f0 -> Scale f0 octave $ V.fromList steps

-- | You can assign 'Scale' to some type. Intention of 'Pitch'
-- type is to provide user with freedom of choice for 
-- musical alphabet. You can give your own names to steps
-- and then assign some default 'Scale' to them.
class HasScale a where
    -- | Constant function that returns 'Scale' 
    -- for same type. 
    defScale :: a -> Scale

-- | Scale value on doubles          
scaleAt :: Scale -> Double -> Hz
scaleAt s x = scaleAtInt s d * bendCoeff s n r 
    where (d, r) = properFraction x          
          n      = mod d $ scaleSize s


-- | 'Pitch' should be used alongside with many
-- other parameters (they can define timbre or loudness). 
-- Class 'PitchLike' provides getters and setters for
-- data types that contain value of type 'Pitch'. 
-- In "Temporal.Music.Track" module you can find many
-- functions that are defined in terms of this class. Once you
-- have chosen some note representation you can make an instance 
-- for it and use all pitch-modifiers.
class PitchLike a where
    type Step a :: *
    setPitch :: Pitch (Step a) -> a -> a
    getPitch :: a -> Pitch (Step a)

-- | Pitch modifier.
mapPitch :: PitchLike a => (Pitch (Step a) -> Pitch (Step a)) -> a -> a
mapPitch f x = setPitch (f $ getPitch x) x

instance PitchLike (Pitch a) where
    type Step (Pitch a) = a
    setPitch = const id
    getPitch = id

-- | Constructs 'Pitch' from some step value. 'Bend' and
-- 'Octave' are set to zeroes. 'Scale' is set to default scale
-- which is defined in 'HasScale' class.
fromStep :: HasScale a => a -> Pitch a
fromStep a = Pitch (defScale a) 0 0 $ Mod a


-- | Gives scale multiplier
scaleStep :: Scale -> Int -> Interval
scaleStep s x = (scaleOctave s ^^ o) * scaleSteps s V.! n    
    where (o, n) = divMod x $ scaleSize s


-- | Gives number of steps in one octave.
scaleSize :: Scale -> Int
scaleSize = V.length . scaleSteps


-- | Transpose cycles per second by some interval.
transpose :: Interval -> Hz -> Hz
transpose k a = k * a

---------------------------------------------------------
-- rendering


absPitch :: (Finite a) => Pitch a -> Hz
absPitch p = pitchScale p `scaleAt` pitchAsDouble p

-- | Calculates cycles per second for a pitch.
hz :: (Finite (Step a), PitchLike a) => a -> Hz
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
            where n = scaleSize s
                  o = scaleOctave s


loginterpCoeff :: (Double, Double) -> Double -> Double
loginterpCoeff (l, r) x = (r / l) ** x


