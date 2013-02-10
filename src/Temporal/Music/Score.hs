-- | Composition and control.
module Temporal.Music.Score(
    -- * Types
    Dur, Score, Event(..), eventEnd, within,
    -- * Composition
    temp, rest, stretch, delay, reflect, (+|), (*|), (=:=), (+:+), (=:/),
    line, chord, chordT, loop, sustain, sustainT,    
    -- * Filtering
    slice, takeS, dropS, filterEvents,    
    -- * Mappings
    mapEvents, tmap, tmapRel,
    -- * Rendering
    dur, render, alignByZero, sortEvents,   
    -- * Miscellaneous
    linfun, linfunRel,
    -- ** Monoid synonyms
    --
    -- | This package heavily relies on 'Monoid's, so there are shorcuts
    -- for 'Monoid' methods.    
    nil,
    module Data.Monoid,
    -- * Volume control
    setDiap, setDiapRel, setLevel, setAccent, accent, (!),
    louder, quieter, loud, quiet, envelope, envelopeSeg, envelopeRel, 
    -- * Pitch control
    setScale, setBend, setStep, step, bend,
    lower, higher, low, high, 
    -- ** Shortcuts
    -- | Denotes @lower 1-2@ and @higher 1-2@.
    l', ll', hh', h',
    -- * Time stretching   
    r, dot, ddot, tri, bpm,

    -- ** Shortcuts
    -- | Naming conventions : 
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
    bn, wn, hn, qn, en, sn, tn,
    dbn, dwn, dhn, dqn, den, dsn, dtn,

    -- ** Pauses
    -- | Naming conventions are the same as for 'time stretching'.
    bnr, wnr, hnr, qnr, enr, snr, tnr,
    dbnr, dwnr, dhnr, dqnr, denr, dsnr, dtnr
    )

where

import Temporal.Media(Event(..), within, eventEnd, nil, 
        linfun, linfunRel, alignByZero, sortEvents)
import qualified Temporal.Media as M
import Temporal.Music.Pitch
import Temporal.Music.Volume
import Data.Monoid
import Data.Foldable

-- | Duration.
type Dur = Double

-- | Instances
--
-- * 'Functor' 'Score'
--
-- * 'Foldable' 'Score'
--
-- * 'Monoid' @(@'Score' @a)@
type Score a = M.Track Double a

-------------------------------------------------------
-- Composition
--

-- | 'temp' constructs just an event. 
-- Value of type @a@ lasts for one time unit and starts at zero.
temp :: a -> Score a
temp = M.temp

-- | Empty 'Score' that lasts for some time.
rest :: Dur -> Score a
rest = M.rest

-- | Delays all events by given duration. 
delay :: Dur -> Score a -> Score a
delay = M.delay

-- | Stretches 'Score' in time domain.
stretch :: Dur -> Score a -> Score a
stretch = M.stretch

-- | Infix 'delay' function.
(+|) :: Dur -> Score a -> Score a
(+|) = delay

-- | Infix 'stretch' function.
(*|) :: Dur -> Score a -> Score a
(*|) = stretch

-- | Reversing the scores
reflect :: Score a -> Score a
reflect = M.reflect

-- | Parallel composition. Play two scores simultaneously.
(=:=) :: Score a -> Score a -> Score a
(=:=) = (M.=:=)
 
-- | Sequent composition. Play first score then second.
(+:+) :: Score a -> Score a -> Score a
(+:+) = (M.+:+)

-- | Turncating parallel composition. Total duration
-- equals to minimum of the two scores. All events
-- that goes beyond the lmimt are dropped.
(=:/) :: Score a -> Score a -> Score a
(=:/) = (M.=:/)

-- | Sequent composition on list of scores.
line :: [Score a] -> Score a
line = M.line

-- | Parallel composition on list of scores.
chord :: [Score a] -> Score a
chord = M.chord

-- | Turncating parallel composition on list of scores.
chordT :: [Score a] -> Score a
chordT = M.chordT

-- | Analog of 'replicate' function for scores. Replicated
-- scores are played sequentially.
loop :: Int -> Score a -> Score a
loop = M.loop 


-- | After this transformation events last longer
-- by some constant amount of time.
sustain :: Dur -> Score a -> Score a
sustain = M.sustain

-- | Prolongated events can not exceed total score duration.
-- All event are sustained but those that are close to 
-- end of the score are clipped. It resembles sustain on piano,
-- when score ends you release the pedal.
sustainT :: Dur -> Score a -> Score a
sustainT = M.sustainT

--------------------------------------------------
-- filtering

-- | 'slice' cuts piece of value within given time interval.
-- for @('slice' t0 t1 m)@, if @t1 < t0@ result is reversed.
-- If @t0@ is negative or @t1@ goes beyond @'dur' m@ blocks of
-- nothing inserted so that duration of result equals to 
-- @'abs' (t0 - t1)@.
slice :: Dur -> Dur -> Score a -> Score a
slice = M.slice

-- | @('takeS' t)@ is equivalent to @('slice' 0 t)@.
takeS :: Dur -> Score a -> Score a
takeS = M.takeT

-- | @('dropS' t m)@ is equivalent to @('slice' t (dur a) a)@.
dropS :: Dur -> Score a -> Score a
dropS = M.dropT

-- | Filter score.
filterEvents :: (Event Dur a -> Bool) -> Score a -> Score a
filterEvents = M.filterEvents

------------------------------------------------------
-- mapping

-- | General mapping. Mapps not only values but events.
mapEvents :: (Event Dur a -> Event Dur b) -> Score a -> Score b
mapEvents = M.mapEvents

-- | Mapps values and time stamps.
tmap :: (Event Dur a -> b) -> Score a -> Score b
tmap = M.tmap

-- | Relative tmap. Time values are normalized by argument's duration. 
tmapRel :: (Event Dur a -> b) -> Score a -> Score b
tmapRel = M.tmapRel

------------------------------------------------------
-- rendering

-- | Calculates duration.
dur :: Score a -> Dur
dur = M.dur

-- | Gets all recordered events. 
render :: Score a -> [Event Dur a]
render = M.render

-------------------------------------------------------
-- Volume control
--

-- | Sets diapason to specified value.
setDiap :: VolumeLike a => (Amp, Amp) -> Score a -> Score a
setDiap a = fmap $ mapVolume $ 
    \v -> let d = volumeDiap v
          in  v{ volumeDiap = d{ diapRange = a } }

-- | Relative update of diapason value in decibels, 
-- (0, 1) turns diapason interval into itself.
setDiapRel :: VolumeLike a => (Double, Double) -> Score a -> Score a
setDiapRel (a, b) = fmap $ mapVolume $ 
    \v -> let d = volumeDiap v
          in  v{ volumeDiap = d{ diapRange = (diapAt d a, diapAt d b) } }


-- | Sets level to the given value.
setLevel :: VolumeLike a => Level -> Score a -> Score a
setLevel a = fmap $ mapVolume $
    \v -> v{ volumeLevel = a }

-- | Sets accent to the given value
setAccent :: VolumeLike a => Accent -> Score a -> Score a
setAccent a = fmap $ mapVolume $
    \v -> v{ volumeAccent = a }

-- | Increases 'Accent' by the given value.
accent :: VolumeLike a => Accent -> Score a -> Score a
accent a = fmap $ mapVolume $
    \v -> v{ volumeAccent = a + volumeAccent v }

-- | Synonym for @flip setAcent@
(!) :: VolumeLike a => Score a -> Accent -> Score a
(!) = flip setAccent

-- | Input becomes louder by given number of levels.
louder :: (VolumeLike a) => Int -> Score a -> Score a
louder n = fmap $ mapVolume $
    \v -> v{ volumeLevel = volumeLevel v + n }

-- | Input becomes quieter by given number of levels.
quieter :: (VolumeLike a) => Int -> Score a -> Score a
quieter = louder . negate

-- | Input becomes one level louder.
loud :: (VolumeLike a) => Score a -> Score a
loud = louder 1

-- | Input becomes one level quieter.
quiet :: (VolumeLike a) => Score a -> Score a
quiet = quieter 1


-- | Accent that depends on time of note, time is relative, 
-- so 'Score' starts at 't = 0' and ends at 't = 1'.
envelope :: (VolumeLike a) => (Dur -> Accent) -> Score a -> Score a
envelope f = tmapRel $ \(Event s d c) -> accent' c (f s)
    where accent' v a = mapVolume (\v -> v{ volumeAccent = a }) v 

-- | 'envelopeSeg' lifts function 'lines' to dynamics level
envelopeSeg :: (VolumeLike a) => [Double] -> Score a -> Score a
envelopeSeg xs = envelope $ (linfun xs)

-- | 'envelopeRel' lifts function 'linesRel' to dynamics level
envelopeRel :: (VolumeLike a) => [Accent] -> Score a -> Score a
envelopeRel xs a = envelope (linfunRel 1 xs) a


---------------------------------------------------------
-- Pitch control

-- | Sets new scale
setScale :: PitchLike a => Scale -> Score a -> Score a
setScale s = fmap $ mapPitch $
    \p -> p{ pitchScale = s }

-- | Sets bend value
setBend :: PitchLike a => Bend -> Score a -> Score a
setBend b = fmap $ mapPitch $ 
    \p -> p{ pitchBend = b }

-- | Increases 'Bend' by given value.
bend :: PitchLike a => Bend -> Score a -> Score a
bend b = fmap $ mapPitch $ 
    \p -> p{ pitchBend = b + pitchBend p }

-- | Sets step value
setStep :: PitchLike a => Step -> Score a -> Score a
setStep s = fmap $ mapPitch $ 
    \p -> p{ pitchStep = s }

-- | Transposition. Increases (octave, step) coordinate by
-- given number of steps.
step :: (PitchLike a) => Int -> Score a -> Score a
step n = fmap $ mapPitch $
    \p -> p{ pitchStep = pitchStep p + n }
    
-- | Transposition by given number of octaves.
higher :: PitchLike a => Int -> Score a -> Score a
higher n = fmap $ mapPitch $ 
    \p -> p{ pitchOctave = pitchOctave p + n }


-- | Transposition by given number of octaves.
lower :: PitchLike a => Int -> Score a -> Score a
lower = higher . negate


-- | One octave higher.
high :: PitchLike a => Score a -> Score a
high = higher 1


-- | One octave lower.
low :: PitchLike a => Score a -> Score a
low = lower 1


l', ll', hh', h' :: PitchLike a => Score a -> Score a

l'   = low
ll'  = lower 2
h'   = high
hh'  = higher 2

{-
-- | inverts note around some tone center. Tone center defines
-- two tones octave apart around current note in wich inversion takes place.
--
-- For example with center at 5 note @c@ in twelve tone scale 
-- @[5, 6, 7, 8, 9, bb, 11, c, 1, 2, 3, 4, 5]@ goes into note  bb.
-- Inversion counts number of steps from lower center tone to given tone
-- and then result is higher center tone shifted lower by this number.
invert :: (Finite (Step a), PitchLike a) => Int -> Score a -> Score a
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
-}  

--------------------------------------------------------------
-- time stretching 
--

-- | Shortcut for 'rest'
r :: Dur -> Score a
r = rest

-- | Means 'triolet'. Plays three notes as fast as two.
tri :: Score a -> Score a
tri = stretch (2/3)


-- | Sets tempo in beats per minute, 
-- if 1 "Dur" is equal to 1 second before transformation.
bpm :: Dur -> (Score a -> Score a)
bpm beat = stretch (x1/x0)
    where x0 = 0.25
          x1 = 60/beat

bn, wn, hn, qn, en, sn, tn  :: Score a -> Score a

bn = stretch 2
wn = id
hn = stretch $ 1/2
qn = stretch $ 1/4
en = stretch $ 1/8
sn = stretch $ 1/16
tn = stretch $ 1/32

dbn, dwn, dhn, dqn, den, dsn, dtn :: Score a -> Score a

-- | Synonym to @'stretch' (3/2)@
dot :: Score a -> Score a
dot = stretch $ 3/2

-- | double 'dot', stretch with 1.75
ddot :: Score a -> Score a
ddot = stretch 1.75

dbn = dot . bn
dwn = dot . wn
dhn = dot . hn
dqn = dot . qn
den = dot . en
dsn = dot . sn
dtn = dot . tn


bnr, wnr, hnr, qnr, enr, snr, tnr :: Score a

wnr = rest 1

bnr = bn wnr
hnr = hn wnr
qnr = qn wnr
enr = en wnr
snr = sn wnr
tnr = tn wnr

dbnr, dwnr, dhnr, dqnr, denr, dsnr, dtnr :: Score a

dbnr = dbn wnr
dwnr = dwn wnr
dhnr = dhn wnr
dqnr = dqn wnr
denr = den wnr
dsnr = dsn wnr
dtnr = dtn wnr

