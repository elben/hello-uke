module Chords where

import Data.Maybe
import Prelude

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
  | Suspended
  | Augmented
  | Diminished

instance chordQualityShow :: Show ChordQuality where
  show Major = "Major"
  show Minor = "Minor"
  show Suspended = "Sus"
  show Augmented = "Aug"
  show Diminished = "Dim"
derive instance chordQualityEq :: Eq ChordQuality
derive instance chordQualityOrd :: Ord ChordQuality

humanChordQuality :: ChordQuality -> String
humanChordQuality Major = ""
humanChordQuality Minor = "m"
humanChordQuality Suspended = "sus"
humanChordQuality Augmented = "aug"
humanChordQuality Diminished = "dim"

chordQualities :: Array ChordQuality
chordQualities = [ Major, Minor, Suspended, Augmented, Diminished ]

data ChordInterval =
    Triad
  -- Seventh
  | Dom7
  | Maj7
  -- Addition
  -- Second and Fourth also works with Suspended quality to make sus2 and sus4.
  | Second
  | Fourth

chordIntervals :: Array ChordInterval
chordIntervals = [ Triad, Dom7, Maj7, Second, Fourth ]

instance chordIntervalShow :: Show ChordInterval where
  show Triad = ""
  show Dom7 = "7"
  show Maj7 = "M7"
  show Second = "2"
  show Fourth = "4"

derive instance chordIntervalEq :: Eq ChordInterval
derive instance chordIntervalOrd :: Ord ChordInterval

-- https://en.wikipedia.org/wiki/Chord_(music)#Examples
humanChordInterval :: ChordInterval -> String
humanChordInterval Triad = ""
humanChordInterval Dom7 = "7"
humanChordInterval Maj7 = "M7"
humanChordInterval Second = "2"
humanChordInterval Fourth = "4"

humanChordMod :: ChordQuality -> ChordInterval -> String
humanChordMod q i =
  let divide = case q of
                 Minor ->
                   case i of
                     Triad -> ""
                     Dom7 -> ""
                     _ -> "/"
                 _ -> ""
  in humanChordQuality q <> divide <> humanChordInterval i

-- Represents a fingering on a string. (F 0) is equivalent to the open string. X means don't
-- play that string. B means ignore the fingering, because the Barre will handle it.
data Finger = F Int
            | B Int
            | X

instance fingerShow :: Show Finger where
  show (F pos) = show pos
  show X = "X"
  show (B pos) = "B" <> show pos
derive instance fingerEq :: Eq Finger
derive instance fingerOrd :: Ord Finger

-- Specifies a barre fingering. The first Int is the left-most string
-- (starting at 0), ranging to the right-most string.
data Barre = Barre Int Int

-- The fingering from left-most string when looking at the fretboard.
data Fingering = Fingering (Maybe Barre) (Array Finger)

getBarre :: Fingering -> Maybe Barre
getBarre (Fingering barre _) = barre

-- Easier way of defining tuples. Precedence is *lower* than List's (:), so that we can create
-- tuples in lists like this:
--
-- 1 ==> 10 : 2 ==> 20 : Nil
--
infix 7 Tuple as ==>

-- Mapping of Note, ChordQuality to the fingering.
ukeChords :: Map Pos (Map ChordQuality (Map ChordInterval Fingering))
ukeChords = M.fromFoldable
    [
    -- C
      0 ==> M.fromFoldable
        [ Major ==> M.fromFoldable
            [ Triad ==> fing 0 0 0 3
            , Dom7  ==> fing 0 0 0 1
            ]
        , Minor ==> M.fromFoldable
            [ Triad ==> fing 0 3 3 3
            ]
        ]

    -- G
    , 7 ==> M.fromFoldable
        [ Major ==> M.fromFoldable
            [ Triad ==> fing 0 2 3 2
            ]
        , Minor ==> M.fromFoldable
            [ Triad ==> fing 0 2 3 1
            ]
        ]

-- TODO!! Add barres
    -- A# / Bb
    , 10 ==> M.fromFoldable
        [ Major ==> M.fromFoldable
            [ Triad ==> fini (F 3) (F 2) (B 1) (B 1) (Barre 2 3)
            , Dom7  ==> fini (B 1) (F 2) (B 1) (B 1) (Barre 0 3)
            ]
        , Minor ==> M.fromFoldable
            [ Triad ==> fing 3 1 1 1
            , Dom7  ==> fing 1 1 1 1
            ]
        , Suspended ==> M.fromFoldable
            [ Second ==> fing 3 0 1 1 ]
        ]
    ]

fini :: Finger -> Finger -> Finger -> Finger -> Barre -> Fingering
fini a b c d barre = Fingering (Just barre) [a, b, c, d]

-- Fingering without barre.
fing :: Int -> Int -> Int -> Int -> Fingering
fing a b c d = Fingering Nothing [intToFinger a, intToFinger b, intToFinger c, intToFinger d]

-- Fingering with barre.
finb :: Int -> Int -> Int -> Int -> Barre -> Fingering
finb a b c d barre = Fingering (Just barre) [intToFinger a, intToFinger b, intToFinger c, intToFinger d]

ukeChord :: ChordQuality
         -> ChordInterval
         -> Int -> Int -> Int -> Int
         -> Tuple (Tuple ChordQuality ChordInterval) Fingering
ukeChord q i a b c d = (q ==> i) ==> Fingering Nothing [intToFinger a, intToFinger b, intToFinger c, intToFinger d]

intToFinger :: Int -> Finger
intToFinger n = if n < 0 then X else F n

findUkeChord :: Pos -> ChordQuality -> ChordInterval -> Maybe Fingering
findUkeChord p q i = M.lookup p ukeChords >>= M.lookup q >>= M.lookup i