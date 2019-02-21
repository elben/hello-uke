module Engine where

import Data.Maybe
import Data.String
import Data.Traversable
import Data.Tuple
import Debug.Trace
import Prelude

import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.Lazy (replicate)
import Data.Semigroup ((<>))
import Data.String.CodeUnits (charAt)
import Effect.Console (log)

import Chords

c  :: Note
c  = Note "C"  0
cs :: Note
cs = Note "C♯" 1
df :: Note
df = Note "Db" 1
d  :: Note
d  = Note "D"  2
ds :: Note
ds = Note "D♯" 3
ef :: Note
ef = Note "Eb" 3
e  :: Note
e  = Note "E"  4
f  :: Note
f  = Note "F"  5
fs :: Note
fs = Note "F♯" 6
gf :: Note
gf = Note "Gb" 6
g  :: Note
g  = Note "G"  7
gs :: Note
gs = Note "G♯" 8
af :: Note
af = Note "Ab" 8
a  :: Note
a  = Note "A"  9
as :: Note
as = Note "A♯" 10
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

posToNote :: Pos -> Note
posToNote pos =
  let choices = fromMaybe Nil (L.index notes pos)
  in fromMaybe (Note "?" pos) (L.index choices 0)

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
-- > chooseNote (Pitch 7 4) (0 : 4 : 7 : Nil)
-- (Tuple Pitch 7 4 2)
--
chooseNote :: Pitch ->
              -- The open string we want to find a note to play on
              List Pos ->
              -- List of notes in chord
              Tuple Pitch Int
              -- Returns the pitch chosen and the index of the given positions
              -- chosen.
chooseNote pitch@(Pitch pos octv) options =
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

-- Choose how to play a chord on the given fretboard.
--
-- Goes through each open string on the instrument and chooses the best note to
-- play on each string.
--
-- Examples:
--
-- Play a D7 on the ukulele:
-- > chooseChord ukulele 2 dom7
-- (Pitch 9 4 : Pitch 0 4 : Pitch 6 4 : Pitch 9 4 : Nil)
--
-- TOOD we need to make this smarter:
-- - Don't duplicate notes of the same octave, if possible.
--   Right now, for Em (log (draw ukulele 4 minorTriad))
--   the algo chooses to play:
--
--   G C E A
--   =======
--   | | | |
--   +—+—+—+
--   | | | ●
--   +—+—+—+
--   | | | |
--   +—+—+—+
--   | ● | |
--
--   Which is technically correct, but duplicates the E note of the same octave
--   twice. It would be better to play that open E string as a G, so that we get
--   both the bottom and top G.
--
-- - log (draw ukulele 5 dom7) <- this is also wrong
--   Because our algo is too simple. We can't just choose the "easiset" thing to
--   play, because sometimes we NEED to make it harder in order to hit some of
--   the other notes in the chord. In this F7 example, the algo skipped playing
--   Eb because no open strings were close to it. We need to manually add that.
--
chooseChord :: Fretboard
            -- The fretboard to play on.
            -> Pos
            -- The root note position.
            -> ChordStructure
            -- The chord structure we want to play.
            -> List Pitch
chooseChord (Fretboard maxFrets opens) pos struct =
  let notes = findChord pos struct
      -- Find the best note for each string.
      chosenPitches =
        foldl
          (\pitches open ->
            let Tuple pitch idx = chooseNote open notes
            in pitch : pitches)
          Nil
          opens
  in -- Reverse it because we were appending to the head.
     L.reverse chosenPitches


-- Utility to repeat a string n times.
repeatString :: String -> Int -> String
repeatString s n =
  foldl (\sum _ -> sum <> s) "" (replicate n 0)

-- Draw the fretboard of the given starting note and chord.
--
-- > log (draw ukulele 5 majorTriad)
draw :: Fretboard -> Pos -> ChordStructure -> String
draw fretboard pos struct =
  (drawFrets fretboard (fretted fretboard (chooseChord fretboard pos struct)))

-- Draw the fretboard of the given list of fret positions.
--
--
-- > log (drawFrets ukulele (fretted ukulele (chooseChord ukulele 5 majorTriad)))
--
-- G C E A
-- =======
-- | | ● |
-- +—+—+—+
-- ● | | |
-- +—+—+—+
-- | | | |
-- +—+—+—+
-- .......
--
drawFrets :: Fretboard -> List Fret -> String
drawFrets (Fretboard numFrets opens) frets =
  let -- Draw a nut line. Example: "+—+—+—+". Decr 1 because the last string
      -- should close with a single "+" instead of "+-"
      drawNut = (repeatString "+—" (L.length frets - 1)) <> "+"
      -- Draw the nth fret, showing if a note should be pressed or not.
      drawFret n =
        (foldl (\sum fret -> if fret == n then sum <> "● " else sum <> "| ") "" frets)
        <> "\n"
        <> drawNut
      -- Draw the top of the instrument, which prints out the notes, and then
      -- the starting nut. Example:
      --
      -- G C E A
      -- =======
      header =
        (foldl (\s (Pitch pos oct) -> s <> getNoteName (posToNote pos) <> " ") "" opens)
        <> "\n"
        -- Decr 1 so we don't have a hanging "=".
        <> repeatString "==" (L.length frets - 1) <> "="
  in header
     <> "\n"
     -- Incr 1 because we don't want to draw fret 0, which are the open strings.
     <> (foldlWithIndex (\i s _ -> s <> drawFret (i + 1) <> "\n") "" (replicate numFrets 0))

-- Fret position
type Fret = Int

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
-- > fretted ukulele (chooseChord ukulele 5 majorTriad)
fretted :: Fretboard -> List Pitch -> List Fret
fretted (Fretboard m opens) pitches =
  map (\(Tuple o p) -> difference o p) (L.zip opens pitches)
