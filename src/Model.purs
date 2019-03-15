module Model where

import Chords as C
import Data.Maybe (Maybe(..))

type FretboardId = Int

data Chord = Chord C.Note C.ChordQuality C.ChordInterval

initialChord :: Chord
initialChord = Chord C.c C.Major C.Triad