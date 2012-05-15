module Temporal.Music(
    -- * Introduction
    -- | This library provides two kinds of musical structures. 
    -- First is general 'Track' representation. It tells how to 
    -- combine musical things together and how they can be transformed. 
    -- Second is just the oposite of the first one, it stands for 
    -- very basic musical structures like 'Pitch', 'Scale', 'Volume'.
    
    module Temporal.Music.Pitch,
    module Temporal.Music.Volume,
    module Temporal.Music.Note,
    module Temporal.Music.Score)

where

import Temporal.Music.Pitch
import Temporal.Music.Volume
import Temporal.Music.Note
import Temporal.Music.Score
