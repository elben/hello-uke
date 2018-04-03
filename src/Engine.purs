module Engine where

import Prelude

import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe
import Data.Traversable
import Data.String

-- Note position. C is position 0, C# and Db are position 1, and so on.
type Pos = Int

-- Represents a certain number of half-step.
type Step = Int

type Octave = Int

data Note = Note String Pos

derive instance eqNote :: Eq Note

instance showNote :: Show Note where
  show (Note name pos) = "Note " <> name <> " " <> show pos

-- Pitch is a note plus an octave. "Middle C" on the piano is C4, or the fourth
-- octave.
data Pitch = Pitch Note Octave

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

majorTriad :: ChordStructure
majorTriad = ChordStructure "Major" (0 : 4 : 7 : Nil)

minorTriad :: ChordStructure
minorTriad = ChordStructure "Minor" (0 : 3 : 7 : Nil)

-- https://en.wikipedia.org/wiki/Seventh_chord
dom7 :: ChordStructure
dom7 = ChordStructure "7" (0 : 4 : 7 : 10 : Nil)

major7 :: ChordStructure
major7 = ChordStructure "7" (0 : 4 : 7 : 11 : Nil)

-- Step `pos` up by `count` half-steps, looping back to Pos 0 as necessary.
step :: Pos -> Step -> Pos
step pos count =
  mod (pos + count) 12

-- Is the given note flat?
isFlat :: Note -> Boolean
isFlat (Note name pos) = length name == 2 && charAt 1 name == Just 'b'

-- Is the given note sharp?
isSharp :: Note -> Boolean
isSharp (Note name pos) = length name == 2 && charAt 1 name == Just '#'

-- Is the given note natural?
isNat :: Note -> Boolean
isNat (Note name pos) = length name == 1

-- Given the root note and the steps above the root note, choose and return the
-- note at the given step. Intelligently decides whether the sharp or flat
-- version of the returned note based off the root note.
choose :: Note -> Step -> Maybe Note
choose root@(Note _ pos) count = do
  choices <- L.index notes (step pos count)
  let idx = if L.length choices == 1
            then 0 -- Only one choice
            else
              if isNat root || isFlat root
              then 1 -- Choose the flat version
              else 0 -- Choose the sharp version
  L.index choices idx

-- findChord :: Note -> ChordStructure -> Maybe (List Note)
findChord root@(Note nname pos) (ChordStructure cname ords) = do
  sequence (map (choose root) ords)

