module Engine where

import Prelude

import Data.List (List(..), (:), index)
import Data.Maybe
import Data.Traversable
import Data.String

type Ord = Int
type Octave = Int

data Note = Note String Ord

derive instance eqNote :: Eq Note

instance showNote :: Show Note where
  show (Note name ord) = "Note " <> name <> " " <> show ord

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
data ChordStructure = ChordStructure String (List Ord)

majorTriad :: ChordStructure
majorTriad = ChordStructure "Major" (0 : 4 : 7 : Nil)

minorTriad :: ChordStructure
minorTriad = ChordStructure "Minor" (0 : 3 : 7 : Nil)

-- https://en.wikipedia.org/wiki/Seventh_chord
dom7 :: ChordStructure
dom7 = ChordStructure "7" (0 : 4 : 7 : 10 : Nil)

major7 :: ChordStructure
major7 = ChordStructure "7" (0 : 4 : 7 : 11 : Nil)

step :: Ord -> Int -> Ord
step ord count =
  mod (ord + count) 12

isFlat :: Note -> Boolean
isFlat (Note name ord) = length name == 2 && charAt 1 name == Just 'b'

isSharp :: Note -> Boolean
isSharp (Note name ord) = length name == 2 && charAt 1 name == Just '#'

steps :: Ord -> List Ord -> List Ord
steps ord ords =
  map (step ord) ords

-- findChord :: Note -> ChordStructure -> Maybe (List Note)
findChord (Note nname ord) (ChordStructure cname ords) =
  let foundOrds = steps ord ords
  in sequence (map (index notes) foundOrds)

