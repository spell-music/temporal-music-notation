{-# LANGUAGE 
       TypeFamilies,
       FlexibleContexts #-}

-- | This module defines the notion of volume.
module Temporal.Music.Volume(
        Amp, Diap(..),
        -- * Volume
        Volume(..), fromLevel, Accent, HasDiap(..),
        -- * VolumeLike
        VolumeLike(..), mapVolume,
        -- * Rendering
        amp, volumeAsDouble, diapAt) 
where

import Data.Finite 

-- | Amplitude.
type Amp = Double

-- | Diapason defines minimum and maximum bound for 'Volume' level.
type Diap = (Amp, Amp)

-- | 'Accent' defines values between 'volumeLevel' values on logarithmic 
-- scale. 1 'Accent' == 1 'volumeLevel' 's step.
type Accent = Double 

-- | 'Volume' denotes 'Amp' value. It's not a 'Double' 
-- for ease of performing some musical transformations, such as
-- making notes louder or using accents. 'Volume' can be converted
-- to 'Amp' with function 'amp'.
data Volume a = Volume {
        volumeDiap      :: Diap,
        volumeAccent    :: Accent,
        volumeLevel     :: Sat a
    } deriving (Show, Eq)


-- | Constructs value of type 'Volume'. 'Accent' is set to zero.
-- Diapason is set to default value (see 'HasDiap' class). 
fromLevel :: HasDiap a => a -> Volume a
fromLevel a = Volume (defDiap a) 0 $ Sat a

-- | Assign default diapason for some type.
class HasDiap a where
    -- | Constant function that returns default diapason.
    defDiap :: a -> Diap


-- | 'Volume' should be used alongside with many
-- other parameters (they can define timbre or pitch). 
-- Class 'VolumeLike' provides getters and setters for
-- data types that contain value of type 'Volume'. 
-- In "Temporal.Music.Track" module you can find many
-- functions that are defined in terms of this class. Once you
-- have chosen some note representation you can make an instance 
-- for it and use all volume-modifiers.
class VolumeLike a where
    type Level a :: *

    setVolume :: Volume (Level a) -> a -> a
    getVolume :: a -> Volume (Level a)


instance VolumeLike (Volume a) where
    type Level (Volume a) = a
    setVolume = const id
    getVolume = id


-- | 'Volume' modifier.
mapVolume :: VolumeLike a =>  
    (Volume (Level a) -> Volume (Level a)) -> (a -> a)
mapVolume f x = setVolume (f (getVolume x)) x

--------------------------------------------
-- rendering
--

absVolume :: Finite a => Volume a -> Amp
absVolume v = diapAt (volumeDiap v) (volumeAsDouble v)

-- | Calculates amplitude for a 'Volume' -like value.
amp :: (Finite (Level a), VolumeLike a) => a -> Amp
amp = absVolume . getVolume

-- | Calculates value of type 'Volume' as coordinate 
-- within specidfied diapason. 1 corresponds to maximum bound 
-- and 0 corresponds to minimum bound.
volumeAsDouble :: Finite a => Volume a -> Double
volumeAsDouble v = sat 0 1 $ ((fromIntegral $ fromEnum l) + a)/fromIntegral c
    where l = volumeLevel v
          a = volumeAccent v  
          c = domLength l

-- | Mapps decibels to amplitudes within specified amplitude 
-- diapason, 0 turns to lower diapason value and 1 turns 
-- to higher diapason value.
diapAt :: Diap -> Double -> Double
diapAt (low, high) x = (low * ) $ (high / low) ** x

