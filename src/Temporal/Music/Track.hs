{-# LANGUAGE 
       FlexibleInstances, 
       FlexibleContexts #-}

-- | Main musical transformations
module Temporal.Music.Track(
    -- * Composition
    -- | Look at "Temporal.Media" module for functions 
    -- that perform composition of tracks.
    Score, module Temporal.Media,
    -- * Volume control
    setDiap, setDiapRel, setLevel, setAccent, accent, (!),
    louder, quieter, loud, quiet, envelope,
    -- * Pitch control
    setScale, setBend, setStep, step, bend,
    lower, higher, low, high, l', ll', hh', h',
    invert,
    -- * Time stretching   
    -- | Shortcuts for time stretching 
    -- Naming conventions : 
    --
    -- First part @x@ can be [b | w | h | q | e | s | t | d[x] ] 
    --
    -- @b@ means brewis @(stretch 2)@
    --
    -- @w@ means whole @(stretch 1)@
    --
    -- @h@ means half @(stretch $ 1/2)@
    --
    -- @q@ means quater @(stretch $ 1/4)@
    --
    -- @e@ means eighth @(stretch $ 1/8)@
    -- 
    -- @s@ means sixteenth @(stretch $ 1/16)@
    -- 
    -- @t@ means thirty second @(stretch $ 1/32)@
    --
    -- @d[x]@ means dotted [x] @(stretch 1.5 $ x)@
    r, dot,
    bn, wn, hn, qn, en, sn, tn,
    dbn, dwn, dhn, dqn, den, dsn, dtn,

    -- * Pauses
    -- | Naming conventions are the same as for 'time stretching'.
    bnr, wnr, hnr, qnr, enr, snr, tnr,
    dbnr, dwnr, dhnr, dqnr, denr, dsnr, dtnr
    )

where

import Temporal.Media 
import Temporal.Music.Pitch
import Temporal.Music.Volume
import Data.Finite


type Score a = Track Double a

-------------------------------------------------------
-- Volume control
--

-- | Sets diapason to specified value.
setDiap :: VolumeLike a => Diap -> Track t a -> Track t a
setDiap a = fmap $ mapVolume $ 
    \v -> v{ volumeDiap = a }

-- | Relative update of diapason value in decibels, 
-- (0, 1) turns diapason interval into itself.
setDiapRel :: VolumeLike a => Diap -> Track t a -> Track t a
setDiapRel (a, b) = fmap $ mapVolume $ 
    \v -> let d = volumeDiap v
          in  v{ volumeDiap = (diapAt d a, diapAt d b) }


-- | Sets level to the given value.
setLevel :: VolumeLike a => Level a -> Track t a -> Track t a
setLevel a = fmap $ mapVolume $
    \v -> v{ volumeLevel = Sat a }

-- | Sets accent to the given value
setAccent :: VolumeLike a => Accent -> Track t a -> Track t a
setAccent a = fmap $ mapVolume $
    \v -> v{ volumeAccent = a }

-- | Increases 'Accent' by the given value.
accent :: VolumeLike a => Accent -> Track t a -> Track t a
accent a = fmap $ mapVolume $
    \v -> v{ volumeAccent = a + volumeAccent v }

-- | Synonym for @flip setAcent@
(!) :: VolumeLike a => Track t a -> Accent -> Track t a
(!) = flip setAccent

-- | Input becomes louder by given number of levels.
louder :: (Finite (Level a), VolumeLike a)
    => Int -> Track t a -> Track t a
louder n = fmap $ mapVolume $
    \v -> v{ volumeLevel = toEnum $ 
            (fromEnum $ volumeLevel v) + n }

-- | Input becomes quieter by given number of levels.
quieter :: (Finite (Level a), VolumeLike a)
    => Int -> Track t a -> Track t a
quieter = louder . negate

-- | Input becomes one level louder.
loud :: (Finite (Level a), VolumeLike a)
    => Track t a -> Track t a
loud = louder 1

-- | Input becomes one level quieter.
quiet :: (Finite (Level a), VolumeLike a) 
    => Track t a -> Track t a
quiet = quieter 1


-- | Accent that depends on time of note
envelope :: (Fractional t, VolumeLike a) 
    => (t -> Accent) -> Track t a -> Track t a
envelope f = tmap $ \(Event s d c) -> accent' c (f s)
    where accent' v a = mapVolume (\v -> v{ volumeAccent = a }) v 

---------------------------------------------------------
-- Pitch control

-- | Sets new scale
setScale :: PitchLike a => Scale -> Track t a -> Track t a
setScale s = fmap $ mapPitch $
    \p -> p{ pitchScale = s }

-- | Sets bend value
setBend :: PitchLike a => Bend -> Track t a -> Track t a
setBend b = fmap $ mapPitch $ 
    \p -> p{ pitchBend = b }

-- | Increases 'Bend' by given value.
bend :: PitchLike a => Bend -> Track t a -> Track t a
bend b = fmap $ mapPitch $ 
    \p -> p{ pitchBend = b + pitchBend p }

-- | Sets step value
setStep :: PitchLike a => Step a -> Track t a -> Track t a
setStep s = fmap $ mapPitch $ 
    \p -> p{ pitchStep = Mod s }

-- | Transposition. Increases (octave, step) coordinate by
-- given number of steps.
step :: (Finite (Step a), PitchLike a) => Int -> Track t a -> Track t a
step n = fmap $ mapPitch $
    \p -> let (o, s) = step' $ pitchStep p
          in  p{ pitchOctave = o + pitchOctave p, 
                 pitchStep   = toEnum s }  
    where step' s = divMod (n + fromEnum s) (domLength s) 


-- | Transposition by given number of octaves.
higher :: PitchLike a => Int -> Track t a -> Track t a
higher n = fmap $ mapPitch $ 
    \p -> p{ pitchOctave = pitchOctave p + n }


-- | Transposition by given number of octaves.
lower :: PitchLike a => Int -> Track t a -> Track t a
lower = higher . negate


-- | One octave higher.
high :: PitchLike a => Track t a -> Track t a
high = higher 1


-- | One octave lower.
low :: PitchLike a => Track t a -> Track t a
low = lower 1


l', ll', hh', h' :: PitchLike a => Track t a -> Track t a

l'   = low
ll'  = lower 2
h'   = high
hh'  = higher 2

-- | inverts note around some tone center. Tone center defines
-- two tones octave apart around current note in wich inversion takes place.
--
-- For example with center at 5 note @c@ in twelve tone scale 
-- @[5, 6, 7, 8, 9, bb, 11, c, 1, 2, 3, 4, 5]@ goes into note  bb.
-- Inversion counts number of steps from lower center tone to given tone
-- and then result is higher center tone shifted lower by this number.
invert :: (Finite (Step a), PitchLike a) => Int -> Track t a -> Track t a
invert center = fmap $ mapPitch $
    \p -> let n = domLength $ pitchStep p
              c = mod center n
              w = fromEnum $ pitchStep p
              q = if c <= w
                  then 2 * c + n - w
                  else 2 * c - n - w
              (o, s) = divMod q n
          in  p{ pitchOctave = o + pitchOctave p,
                 pitchStep   = toEnum s }  
    

--------------------------------------------------------------
-- time stretching 
--

-- | Shortcut for 'rest'
r :: Time t => t -> Track t a
r = rest

bn, wn, hn, qn, en, sn, tn  :: 
    (Fractional t, Time t) => Track t a -> Track t a

bn = stretch 2
wn = id
hn = stretch $ 1/2
qn = stretch $ 1/4
en = stretch $ 1/8
sn = stretch $ 1/16
tn = stretch $ 1/32

dbn, dwn, dhn, dqn, den, dsn, dtn :: 
    (Fractional t, Time t) => Track t a -> Track t a

-- | Synonym to @'stretch' (3/2)@
dot :: (Fractional t, Time t) => Track t a -> Track t a
dot = stretch $ 3/2

dbn = dot . bn
dwn = dot . wn
dhn = dot . hn
dqn = dot . qn
den = dot . en
dsn = dot . sn
dtn = dot . tn


bnr, wnr, hnr, qnr, enr, snr, tnr ::
    (Fractional t, Time t) => Track t a

wnr = rest 1

bnr = bn wnr
hnr = hn wnr
qnr = qn wnr
enr = en wnr
snr = sn wnr
tnr = tn wnr

dbnr, dwnr, dhnr, dqnr, denr, dsnr, dtnr ::
    (Fractional t, Time t) => Track t a

dbnr = dbn wnr
dwnr = dwn wnr
dhnr = dhn wnr
dqnr = dqn wnr
denr = den wnr
dsnr = dsn wnr
dtnr = dtn wnr

