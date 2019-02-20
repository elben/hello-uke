module Chords where

import Prelude

import Data.List (List)
import Data.Map (Map)
import Data.Map as M

-- Note position. C is position 0, C# and Db are position 1, and so on.
type Pos = Int

-- Represents a certain number of half-step.
type Step = Int

type Octave = Int

-- Consider this a "named note". We don't want to use this until the very end of
-- our computation, when we finally need to name notes for display.
data Note = Note String Pos

getNoteName :: Note -> String
getNoteName (Note name _) = name

derive instance eqNote :: Eq Note

instance showNote :: Show Note where
  show (Note name pos) = "Note " <> name <> " " <> show pos

-- Pitch is a position plus an octave. "Middle C" on the piano is C4, or the
-- fourth octave.
data Pitch = Pitch Pos Octave

derive instance eqPitch :: Eq Pitch

instance showPitch :: Show Pitch where
  show (Pitch pos oct) = "Pitch " <> show pos <> " " <> show oct

-- Define the various chords we can make. Though the precise definition of "chord quality" differs than the ones listed
-- here, we use "chord quality" to mean various ways we currently support a chord can be created.
--
-- References:
--
-- https://en.wikipedia.org/wiki/Chord_(music)#Symbols
-- https://en.wikipedia.org/wiki/Chord_(music)#Basics
-- https://en.wikipedia.org/wiki/Interval_(music)#Quality
--
data ChordQuality =
    Major
  | Minor
  | Dom7
  | Maj7
  | Sus2
  | Sus4
--   | Augmented
--   | Diminished

-- Chord for a standard-tuned ukulele.
-- The list of positions represents the
data UkeChord = Chord Note ChordQuality (List Pos)

-- Mapping of Note -> ChordQuality -> UkeChord.
ukeChords :: Map Pos (Map ChordQuality UkeChord)
ukeChords = M.empty