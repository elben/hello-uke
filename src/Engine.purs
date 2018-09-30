module Engine where

import Prelude

import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe
import Data.Traversable
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.String
import Data.String.CodeUnits (charAt)
import Data.Tuple
import Debug.Trace

-- Note position. C is position 0, C# and Db are position 1, and so on.
type Pos = Int

-- Represents a certain number of half-step.
type Step = Int

type Octave = Int

-- Consider this a "named note". We don't want to use this until the very end of
-- our computation, when we finally need to name notes for display.
data Note = Note String Pos

derive instance eqNote :: Eq Note

instance showNote :: Show Note where
  show (Note name pos) = "Note " <> name <> " " <> show pos

-- Pitch is a position plus an octave. "Middle C" on the piano is C4, or the
-- fourth octave.
data Pitch = Pitch Pos Octave

derive instance eqPitch :: Eq Pitch

instance showPitch :: Show Pitch where
  show (Pitch pos oct) = "Pitch " <> show pos <> " " <> show oct

c  :: Note
c  = Note "C"  0
cs :: Note
cs = Note "C#" 1
df :: Note
df = Note "Db" 1
d  :: Note
d  = Note "D"  2
ds :: Note
ds = Note "D#" 3
ef :: Note
ef = Note "Eb" 3
e  :: Note
e  = Note "E"  4
f  :: Note
f  = Note "F"  5
fs :: Note
fs = Note "F#" 6
gf :: Note
gf = Note "Gb" 6
g  :: Note
g  = Note "G"  7
gs :: Note
gs = Note "G#" 8
af :: Note
af = Note "Ab" 8
a  :: Note
a  = Note "A"  9
as :: Note
as = Note "A#" 10
bf :: Note
bf = Note "Bb" 10
b  :: Note
b  = Note "B"  11

notes :: List (List Note)
notes = (
    (c : Nil)
  : (cs : df : Nil)
  : (d : Nil)
  : (ds : ef : Nil)
  : (e : Nil)
  : (f : Nil)
  : (fs : gf : Nil)
  : (g : Nil)
  : (gs : af : Nil)
  : (a : Nil)
  : (as : bf : Nil)
  : (b : Nil)
  : Nil)

-- ChordStructure explains how a specific chord can be built. The list of
-- numbers represent the half-steps required to build the chord.
data ChordStructure = ChordStructure String (List Pos)

instance showChordStructure :: Show ChordStructure where
  show (ChordStructure name positions) = "ChordStructure " <> name <> " " <> show positions

majorTriad :: ChordStructure
majorTriad = ChordStructure "Major" (0 : 4 : 7 : Nil)

minorTriad :: ChordStructure
minorTriad = ChordStructure "Minor" (0 : 3 : 7 : Nil)

-- https://en.wikipedia.org/wiki/Seventh_chord
dom7 :: ChordStructure
dom7 = ChordStructure "7" (0 : 4 : 7 : 10 : Nil)

major7 :: ChordStructure
major7 = ChordStructure "M7" (0 : 4 : 7 : 11 : Nil)

-- Step `pos` up by `count` half-steps, looping back to Pos 0 as necessary.
--
-- > step 1 3
-- 4
--
-- > step 11 3
-- 2
--
step :: Pos -> Step -> Pos
step pos count =
  mod (pos + count) 12

-- Find the chord positions for the given chord structure and a starting
-- position (the root note).
--
-- > findChord 0 majorTriad
-- (0 : 4 : 7 : Nil)
--
-- > findChord 5 major7
-- (5 : 9 : 0 : 4 : Nil)
--
findChord :: Pos -> ChordStructure -> List Pos
findChord pos (ChordStructure cname poses) =
  map (step pos) poses

-- Fretboard consists of:
-- * number of frets
-- * the strings and their open tunings
data Fretboard = Fretboard Int (List Pitch)

instance showFretboard :: Show Fretboard where
  show (Fretboard maxFrets pitches) = "Fretboard " <> show maxFrets <> " " <> show pitches

ukulele :: Fretboard
ukulele = Fretboard 13 (Pitch 7 4 : Pitch 0 4 : Pitch 4 4 : Pitch 9 4 : Nil)

-- Fret position
type Fret = Int

-- Calculate the number of steps between the first position to the position,
-- going up the scale.
--
-- For example:
-- * C (pos 0) and a E (pos 4) are four positions away.
-- * Bb (pos 10) and D (pos 2) are also four positions away.
--
-- > distance 0 4
-- 4
--
-- > distance 10 2
-- 4
--
distance :: Pos -> Pos -> Pos
distance p1 p2 =
  if p2 >= p1
  then p2 - p1
  else (p2 + 12) - p1

-- Take a pitch and tune it `steps` up.
tuneUp :: Pitch -> Step -> Pitch
tuneUp (Pitch ppos octave) steps =
  let total = (ppos + (12 * octave)) + steps
  in Pitch (mod total 12) (total / 12)

-- Given an open string, choose which note to play on that string.
--
-- This uses a solver that minimizes distance from the given pitches of the
-- fretboard, and the availability of each string.
--
-- As an example, say we are on a Ukulele, which has a default open tuning of
-- GCEA. Now we want to play the C7 chord, which has positions {0, 4, 7, 10}.
-- This method will take a string (say A), and find the note in the C major
-- chord that would be *easiest* to play on the open A. In this case, the method
-- would return Bb (pos 10) because it's a half-step away from A.
--
-- This simple algorithm of choosing the closest note works pretty well, and
-- from my findings we don't have to actually track how many times each notes in
-- the chord has been selected for play. The reason for this, I suspect, is that
-- the open strings of an instrument were careful selected with regards to their
-- distances among each other.
--
-- Ignoring the "usage" of notes also allows the algorithm flexibility in
-- playing notes more than once, or none at all, which is acceptable on an
-- instrument like the ukulele, which only has 4 open strings.
--
-- > chooseNoteForString (Pitch 7 4) (0 : 4 : 7 : Nil)
-- (Tuple Pitch 7 4 2)
--
chooseNoteForString :: Pitch ->
          -- The open string we want to find a note to play on
          List Pos ->
          -- List of notes in chord
          Tuple Pitch Int
          -- Returns the pitch chosen and the index of the given positions
          -- chosen.
chooseNoteForString pitch@(Pitch pos octv) options =
  -- Find the cost of getting the open string to hit the supplied notes. costMap
  -- is a List of distances.
  let costMap = map (distance pos) options

  -- Find the note that would play "easiest" on the given string, where easiest
  -- is defined by the lowest number of frets from the nut.
  --
  -- `incr` is number of steps from the open string needed to play the chosen
  -- note on the given string. `idx` is the index of the chosen note in
  -- `options`.
      Tuple incr idx = foldlWithIndex
         (\i (Tuple minCost minIdx) cost ->
           if cost < minCost
             then Tuple cost i
             else Tuple minCost minIdx)
         (Tuple 999999 (-1))
         costMap
   in Tuple (tuneUp pitch incr) idx

chooseChord :: Fretboard -> Pos -> ChordStructure -> Fretboard
chooseChord (Fretboard maxFrets opens) pos struct =
  let notes = findChord pos struct
      chosenPitches =
        foldl
          (\pitches open ->
            let Tuple pitch idx = chooseNoteForString open notes
            in pitch : pitches)
          Nil
          opens
  in Fretboard maxFrets (L.reverse chosenPitches)


-- Calculates how many frets you have to play up.
difference :: Pitch -- Open string
           -> Pitch -- Pitch to aim for
           -> Fret  -- Number of frets required
difference (Pitch pos1 octv1) (Pitch pos2 octv2) =
  ((octv2 * 12) + pos2) - ((octv1 * 12) + pos1)

-- Takes the open fretboard, the pitches on the fretboard you want to hit, and
-- returns the list of Frets.
--
-- F major chord:
-- fretted ukulele (chooseChord ukulele 5 majorTriad)
fretted :: Fretboard -> Fretboard -> List Fret
fretted (Fretboard m opens) (Fretboard _ pitches) =
  map (\(Tuple o p) -> difference o p) (L.zip opens pitches)
