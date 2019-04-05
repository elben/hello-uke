module Notes where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe, fromMaybe)

-- Note position. C is position 0, C# and Db are position 1, and so on.
type Pos = Int

-- Represents a certain number of half-step.
type Step = Int

data Accidental = Natural | Sharp | Flat

derive instance eqAccidental :: Eq Accidental

instance showAccidental :: Show Accidental where
  show Natural = ""
  show Sharp = "♯"
  show Flat = "♭"

-- Neutral keys have a default accidental. (E.g. C is flat, G is sharp).
defaultAccidental :: Pos -> Accidental
defaultAccidental 0 = Flat
defaultAccidental 2 = Sharp
defaultAccidental 4 = Sharp
defaultAccidental 5 = Flat
defaultAccidental 7 = Sharp
defaultAccidental 9 = Sharp
defaultAccidental 11 = Flat
defaultAccidental _ = Sharp


-- A Note consists of its letter, accidental, and position.
data Note = Note String Accidental Pos

c  :: Note
c  = Note "C" Natural 0
cs :: Note
cs = Note "C" Sharp   1
df :: Note
df = Note "D" Flat    1
d  :: Note
d  = Note "D" Natural 2
ds :: Note
ds = Note "D" Sharp   3
ef :: Note
ef = Note "E" Flat    3
e  :: Note
e  = Note "E" Natural 4
f  :: Note
f  = Note "F" Natural 5
fs :: Note
fs = Note "F" Sharp   6
gf :: Note
gf = Note "G" Flat    6
g  :: Note
g  = Note "G" Natural 7
gs :: Note
gs = Note "G" Sharp   8
af :: Note
af = Note "A" Flat    8
a  :: Note
a  = Note "A" Natural 9
as :: Note
as = Note "A" Sharp   10
bf :: Note
bf = Note "B" Flat    10
b  :: Note
b  = Note "B" Natural 11

allNotes :: Array Note
allNotes = [c, cs, df, d, ds, ef, e, f, fs, gf, g, gs, af, a, as, bf, b]

notes :: Array (Array Note)
notes = [
    [c]      -- 0
  , [cs, df] -- 1
  , [d]      -- 2
  , [ds, ef] -- 3
  , [e]      -- 4
  , [f]      -- 5
  , [fs, gf] -- 6
  , [g]      -- 7
  , [gs, af] -- 8
  , [a]      -- 9
  , [as, bf] -- 10
  , [b]      -- 11
  ]

getNoteName :: Note -> String
getNoteName (Note name _ _) = name

humanNote :: Note -> String
humanNote (Note name acc pos) = name <> show acc

derive instance eqNote :: Eq Note

instance showNote :: Show Note where
  show (Note name acc pos) = "Note " <> name <> show acc <> " " <> show pos

getNoteAccidental :: Note -> Accidental
getNoteAccidental (Note _ accidental _) = accidental

posToNote :: Pos -> Note
posToNote pos =
  let choices = fromMaybe [] (A.index notes pos)
  in fromMaybe (Note "?" Natural pos) (A.index choices 0)

-- Find the note for the given root key's accidental. For example, if we are in G major,
-- our accidental would be Sharp for G. So if the Pos we are looking for is 6 (F# or Gb),
-- we should choose F#.
findNoteForAccidental :: Pos
                      -> Accidental
                      -> Maybe Note
findNoteForAccidental pos accidental = do
  choices <- A.index notes pos
  if A.length choices == 1
    -- If there's only one choice, it's the Natural note. We don't care about which accidental we
    -- came from.
    then A.index choices 0
    -- If there are multiple choices, choose the one that fits the accidental of the root key.
    else A.find (\(Note _ acc p) -> p == pos && acc == accidental) choices