{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}

-- | This module gives an example of complete musical structure.
-- It defines the notion of note. 
module Temporal.Music.Note(
        -- * Types
        Note(..), note, 
        Drum(..), bam,
        -- * Shortcuts
        -- ** Note shortcuts
        n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, 
        n12, n13, n14, n15, n16, n17, n18, n19, n20, n21, n22, n23,
        -- ** Drum shortcuts
        -- | See "Temporal.Music.Score" shortcuts for naming conventions.
        bd, wd, hd, qd, ed, sd, td, 
        dbd, dwd, dhd, dqd, ded, dsd, dtd
) where

import Data.Default

import Temporal.Music.Score(Score, temp, accent,
        dot, bn, hn, qn, en, sn, tn)
import Temporal.Music.Volume
import Temporal.Music.Pitch

-- note

-- | 'Note' has volume, pitch and some timbral paramters.
data Note a = Note {
        noteVolume  :: Volume,
        notePitch   :: Pitch,
        noteParam   :: Maybe a
    } deriving (Show, Eq)


instance PitchLike (Note a) where
    setPitch p a    = a{ notePitch = p }
    getPitch        = notePitch

instance VolumeLike (Note a) where
    setVolume v a   = a{ noteVolume = v }
    getVolume       = noteVolume

instance Default (Note a) where
    def = Note def def def

-- | Constructs default 'Note' with given step value.
note :: Step -> Score (Note a)
note a = temp $ Note def def{ pitchStep = a } def


-- drum

-- | 'Drum' has only pitch and some timbral paramters.
data Drum a = Drum {
        drumVolume  :: Volume,
        drumParam   :: Maybe a
    } deriving (Show, Eq)

instance VolumeLike (Drum a) where
    setVolume v a   = a{ drumVolume = v }
    getVolume       = drumVolume

instance Default (Drum a) where
    def = Drum def def 

-- | Constructs drum note with given accent. Level is set to the default
-- value.
bam :: Accent -> Score (Drum a)
bam a = accent a $ temp def


--------------------------------------------------------------
-- notes
--

n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, 
    n12, n13, n14, n15, n16, n17, n18, n19, n20, n21, n22, n23
    :: Score (Note a)

n0 = note 0;    n1 = note 1;    n2 = note 2;    n3 = note 3;
n4 = note 4;    n5 = note 5;    n6 = note 6;    n7 = note 7;
n8 = note 8;    n9 = note 9;    n10 = note 10;  n11 = note 11;

n12 = note 12;  n13 = note 13;  n14 = note 14;  n15 = note 15;
n16 = note 16;  n17 = note 17;  n18 = note 18;  n19 = note 19;
n20 = note 20;  n21 = note 21;  n22 = note 22;  n23 = note 23;

--------------------------------------------------------------
-- drums
--

bd, wd, hd, qd, ed, sd, td :: Accent -> Score (Drum a)

bd = bn . bam
wd = bam
hd = hn . bam
qd = qn . bam
ed = en . bam
sd = sn . bam 
td = tn . bam

dbd, dwd, dhd, dqd, ded, dsd, dtd :: Accent -> Score (Drum a) 

dbd = dot . bd
dwd = dot . wd
dhd = dot . hd
dqd = dot . qd
ded = dot . ed
dsd = dot . sd
dtd = dot . td

