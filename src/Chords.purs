module Chords where

import Prelude

import Data.Maybe
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Tuple (Tuple(..))

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
  -- | Sus2
  -- | Sus4
  -- | Augmented
  -- | Diminished

derive instance chordQualityEq :: Eq ChordQuality
derive instance chordQualityOrd :: Ord ChordQuality

-- Represents a fingering on a string. Finger 0 is equivalent to the open string. NoPlay means don't
-- play that string.
data Finger = Finger Int
            | None

instance fingerShow :: Show Finger where
  show (Finger pos) = show pos
  show None = "x"
derive instance fingerEq :: Eq Finger
derive instance fingerOrd :: Ord Finger

-- The fingering from left-most string when looking at the fretboard.
type Fingering = List Finger

-- Easier way of defining tuples. Precedence is *lower* than List's (:), so that we can create
-- tuples in lists like this:
--
-- 1 ==> 10 : 2 ==> 20 : Nil
--
infix 7 Tuple as ==>

-- Mapping of Note, ChordQuality to the fingering.
ukeChords :: Map Pos (Map ChordQuality Fingering)
ukeChords = M.fromFoldable $
    -- C
      0 ==>
        M.fromFoldable (
          Major ==> (Finger 0 : Finger 0 : Finger 0 : Finger 3 : Nil)
        : Minor ==> (Finger 0 : Finger 3 : Finger 3 : Finger 3 : Nil)
        : Nil)

    -- G
    : 7 ==>
        M.fromFoldable (
          Major ==> (Finger 0 : Finger 2 : Finger 3 : Finger 2 : Nil)
        : Minor ==> (Finger 0 : Finger 2 : Finger 3 : Finger 1 : Nil)
        : Nil)
    : Nil

findUkeChord :: Pos -> ChordQuality -> (Maybe Fingering)
findUkeChord p q = M.lookup p ukeChords >>= M.lookup q