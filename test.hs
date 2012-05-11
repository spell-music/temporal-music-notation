import Temporal.Music

type ChordType = [Tone N12]

majCh, minCh :: ChordType

majCh = [0, 4, 7]
minCh = [0, 3, 7]

arpeggi :: Tone P12 -> ChordType -> Score12 ()
arpeggi baseNote chordType = line $ map return $ pchs
    where pchs = map ((+ baseNote) . (chordType !! )) [0, 1, 2, 1, 2, 1]

harmony = line $ map return
     [(4, minCh), (low 9, minCh), (3, majCh),      (7, majCh),
      (0, majCh), (5, minCh),     (low 11, majCh), (4, minCh)]

sco = harmony >>= uncurry arpeggi 

main = print $ dur sco
