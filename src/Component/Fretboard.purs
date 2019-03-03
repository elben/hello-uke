module Component.Fretboard where

import Chords
import Prelude

import Component.Common as Com
import Data.Array (index, range, snoc)
import Data.List (foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Engine (step)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data State = NoChord
           | Chord Note ChordQuality ChordInterval Fingering

humanChord :: State -> String
humanChord NoChord = ""
humanChord (Chord n q i _) = humanNote n <> humanChordMod q i

data Query a
  = ChordChange Note ChordQuality ChordInterval a
  | ClearChord a
  | IsOn (Boolean -> a)

data Input
  = NoChordInput
  | ChordInput Note ChordQuality ChordInterval

data Message = Toggled Boolean

-- Figure out the barre classes.
barreClassNames :: Int   -- String position
                -> Barre
                -> Array ClassName
barreClassNames stringPos (Barre barreFret start end) 
  | stringPos == start = [ClassName "barre", ClassName "first"]
  | stringPos == end   = [ClassName "barre", ClassName "last"]
  | stringPos >= start && stringPos <= end = [ClassName "barre"]
  | otherwise = []

-- Returns true if the given string and fret position has a barre on it.
barreOnFret :: Int         -- String position
            -> Int         -- Fret position
            -> Maybe Barre -- Barre
            -> Boolean
barreOnFret stringPos fretPos (Just barre@(Barre barreFretPos first last)) =
  barreFretPos == fretPos && stringPos >= first && stringPos <= last
barreOnFret _ _ _ = false

-- Renders a circle with text inside.
renderCircle :: forall p i.
                Array ClassName -- CSS classes
             -> Maybe String    -- Text to go in center
             -> HH.HTML p i
renderCircle classes text =
  HH.span
    [ HP.classes ([ClassName "circle"] <> classes) ]
    [ HH.span
        [ HP.classes [ClassName "circle-info"] ]
        [ HH.text (fromMaybe "" text) ]
    ]

-- Renders (or not) a fret for the given string and fret position.
renderFret :: forall p i.
              Int         -- String position
           -> Pos         -- Root note on string
           -> Int         -- Fret position to render
           -> Accidental  -- The chosen key's accidental
           -> Finger      -- Finger on this string to render
           -> Maybe Barre -- Barre
           -> Maybe (HH.HTML p i)
renderFret stringPos rootPos fretPos acc fing barre =
  if not (isBarreOnFret || (getFingerPos fing) == fretPos)
    then Nothing -- Neither a fret nor a finger is put in this string/fret position.
    else
      let classes = if isBarreOnFret then maybe [] (barreClassNames stringPos) barre else []
          text = case fing of
                   -- If fret in (stringPos, fretPos) is an unplayed barre, don't show note
                   -- because another finger (F n) will be playing this string instead.
                   F n -> if isBarreOnFret && higherFingerPlaying barre n
                            then Nothing
                            else map humanNote (findNoteForAccidental (step n rootPos) acc)

                   X -> Just "X"
      in Just (renderCircle classes text)
  where
    isBarreOnFret = barreOnFret stringPos fretPos barre

    -- Check to see if the given finger pos is "higher" up the fretboard than the
    -- barre. This implies that the finger note will be playing over the barre
    -- note.
    higherFingerPlaying :: Maybe Barre -> Int -> Boolean
    higherFingerPlaying bar fingerPos =
      case bar of
        Just (Barre barreFretPos _ _) -> fingerPos > barreFretPos
        _ -> true

renderChordInfo :: forall p i. State -> HH.HTML p i
renderChordInfo s =
  let htmls = case s of
                NoChord -> []
                (Chord n q i _) -> Com.chordHtml n q i
  in HH.div
       [ HP.classes [ClassName "chord-info"] ]
       htmls

-- Determine the number of frets to draw for this state. Draw at least four frets (including the one
-- behind the nut).
numFretsToRender :: State -> Int
numFretsToRender NoChord = 4
numFretsToRender (Chord p q i (Fingering barre fs)) =
  -- Draw at least 4 frets, including the open string fret (the one behind the nut)
  max 4
    ((foldl
      (\m f -> case f of
                X -> m
                F n -> max m n)
      0
      fs) + 1)

-- Draw a string on the instrument, drawing the frets of each string.
renderString :: forall p i.
                State
             -> Int -- n-th string (0 is the left-most string)
             -> Pos -- Root note of string
             -> HH.HTML p i
renderString s stringPos rootPos =
  let fing  = case s of
                 NoChord -> X
                 Chord n q i (Fingering _ fs) -> fromMaybe X (index fs stringPos)
      barre = case s of
                NoChord -> Nothing
                Chord n q i f -> getBarre f
      acc   = case s of
                NoChord -> Natural
                Chord (Note _ a p) q i f -> if a == Natural then defaultAccidental p else a
  in HH.span [ HP.classes [ClassName "string"] ]
       (renderFrets stringPos rootPos (numFretsToRender s) acc fing barre)

-- Renders the frets of a string.
renderFrets :: forall p i.
               Int         -- String position
            -> Pos         -- Root note on string
            -> Int         -- Number of frets to render
            -> Accidental  -- The chosen key's accidental
            -> Finger      -- Finger to be played for this fret on this string
            -> Maybe Barre -- Possible barre
            -> Array (HH.HTML p i)
renderFrets stringPos rootPos numFrets acc fing barre =
  foldl
    (\htmls fretPos ->
      snoc htmls
            (HH.span
              [ HP.classes [ClassName "fret"] ]
              (maybe [] (\h -> [h]) (renderFret stringPos rootPos fretPos acc fing barre))))
    []
    (range 0 (numFrets - 1))

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: initialState
    , render
    , eval
    , receiver
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ HP.classes [ClassName "fretboard"] ]
      [ renderChordInfo state
      , renderString state 0 7 -- G
      , renderString state 1 0 -- C
      , renderString state 2 4 -- E
      , renderString state 3 9 -- A
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    ChordChange note@(Note name acc pos) q i next -> do
      let s = case findUkeChord pos q i of
                 Just fingering -> (Chord note q i fingering)
                 _ -> NoChord
      H.put s
      pure next
    ClearChord next -> do
      H.put NoChord
      pure next
    IsOn reply -> do
      pure (reply true)

  initialState :: Input -> State
  initialState input =
    case input of
      NoChordInput -> NoChord
      ChordInput note@(Note name acc pos) q i ->
        case findUkeChord pos q i of
          Just fingering -> (Chord note q i fingering)
          _ -> NoChord
  
  -- This component receives an Input from the parent component
  receiver :: Input -> Maybe (Query Unit)
  receiver input =
    case input of
      NoChordInput -> Just (ClearChord unit)
      ChordInput p q i -> Just (ChordChange p q i unit)