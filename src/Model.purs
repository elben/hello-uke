module Model where

import Chords as C

type FretboardId = Int

type Chord = { note :: C.Note
             , quality :: C.ChordQuality
             , interval :: C.ChordInterval
             }
initialChord :: Chord
initialChord = { note: C.c, quality: C.Major, interval: C.Triad }

buildChord :: C.Note -> C.ChordQuality -> C.ChordInterval -> Chord
buildChord n q i = { note: n, quality: q, interval: i }