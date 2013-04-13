-- | This module defines the notion of volume.
module Temporal.Music.Volume(
        -- * Types
        Diap(..), Volume(..), Level, Accent,
        -- * VolumeLike
        VolumeLike(..), mapVolume,
        -- * Rendering
        absVolume, volumeAsDouble, diapAt) 
where

import Data.Default

-- | Diapason defines minimum and maximum bound for 'Volume' level.
-- Field 'diapLim' specifies volume limit. Value 'diapLim' is 
-- rendered to highest amplitude and @-diapLim@ is rendered to 
-- the lowest amplitude. All values that go beyond the limit are clipped.
data Diap = Diap
    { diapRange :: (Double, Double)
    , diapLim   :: Int
    } deriving (Show, Eq)


instance Default Diap where
    def = Diap (1e-5, 1) 5

-- | 'Accent' defines values between 'volumeLevel' values on logarithmic 
-- scale. 1 'Accent' == 1 'volumeLevel' 's step.
type Accent = Double 

-- | Volume levels.
type Level  = Int

-- | 'Volume' denotes amplitude. It's not a 'Double' 
-- for ease of performing some musical transformations, such as
-- making notes louder or using accents. 'Volume' can be converted
-- to 'Double' with function 'amp'.
data Volume = Volume {
        volumeDiap      :: Diap,
        volumeAccent    :: Accent,
        volumeLevel     :: Level
    } deriving (Show, Eq)


instance Default Volume where
    def = Volume def def def

-- | 'Volume' can be used alongside with many
-- other parameters (they can define timbre or pitch). 
-- Class 'VolumeLike' provides getters and setters for
-- data types that contain value of type 'Volume'. 
-- In "Temporal.Music.Score" module you can find many
-- functions that are defined in terms of this class. Once you
-- have chosen some note representation you can make an instance 
-- for it and use all volume-modifiers.
class VolumeLike a where
    setVolume :: Volume -> a -> a
    getVolume :: a -> Volume


instance VolumeLike Volume where
    setVolume = const id
    getVolume = id


-- | 'Volume' modifier.
mapVolume :: VolumeLike a =>  (Volume -> Volume) -> (a -> a)
mapVolume f x = setVolume (f (getVolume x)) x

--------------------------------------------
-- rendering
--

renderVolume :: Volume -> Double
renderVolume v = diapAt (volumeDiap v) (volumeAsDouble v)

-- | Calculates amplitude for a 'Volume' -like value.
absVolume :: (VolumeLike a) => a -> Double
absVolume = renderVolume . getVolume

-- | Calculates value of type 'Volume' as coordinate 
-- within specidfied diapason. 1 corresponds to maximum bound 
-- and 0 corresponds to minimum bound.
volumeAsDouble :: Volume -> Double
volumeAsDouble v = 0.5 + d / 2
    where l = volumeLevel v
          a = volumeAccent v  
          c = (diapLim $ volumeDiap v)
          d = sat (-1) 1 $ (fromIntegral l + a) / fromIntegral c

sat :: Ord a => a -> a -> a -> a
sat a b x 
    | x < a = a
    | x > b = b
    | otherwise = x


-- | Mapps decibels to amplitudes within specified amplitude 
-- diapason, 0 turns to lower diapason value and 1 turns 
-- to higher diapason value.
diapAt :: Diap -> Double -> Double
diapAt = diapAt' . diapRange
    where diapAt' (low, high) x = (low * ) $ (high / low) ** x

