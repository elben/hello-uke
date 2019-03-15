module Model where

import Chords as C

type FretboardId = Int

data Chord = Chord C.Note C.ChordQuality C.ChordInterval