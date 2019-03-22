module Model where

import Chords as C

type FretboardId = Int

-- data Chord = Chord C.Note C.ChordQuality C.ChordInterval

type Chord = { note :: C.Note
             , quality :: C.ChordQuality
             , interval :: C.ChordInterval
             }

-- getNote :: Chord -> C.Note
-- getNote (Chord n _ _) = n

-- getQuality :: Chord -> C.ChordQuality
-- getQuality (Chord _ c _) = c

-- getInterval :: Chord -> C.ChordInterval
-- getInterval (Chord _ _ i) = i

initialChord :: Chord
initialChord = { note: C.c, quality: C.Major, interval: C.Triad }

buildChord :: C.Note -> C.ChordQuality -> C.ChordInterval -> Chord
buildChord n q i = { note: n, quality: q, interval: i }