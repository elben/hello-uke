module Component.Fretboard where

import Chords
import Prelude

import Component.Common as Com
import Data.Array (index, range, snoc)
import Data.List (foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence)
import Engine (posToNote, step)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data State = NoChord
           | Chord Pos ChordQuality ChordInterval Fingering

humanChord :: State -> String
humanChord NoChord = ""
humanChord (Chord p q i _) = getNoteName (posToNote p) <> humanChordMod q i

data Query a
  = ChordChange Pos ChordQuality ChordInterval a
  | ClearChord a
  | IsOn (Boolean -> a)

data Input
  = NoChordInput
  | ChordInput Pos ChordQuality ChordInterval

data Message = Toggled Boolean

barreClassNames :: Int -> Barre -> Array ClassName
barreClassNames stringPos (Barre barreFret start end) 
  | stringPos == start = [ClassName "barre", ClassName "first"]
  | stringPos == end   = [ClassName "barre", ClassName "last"]
  | stringPos >= start && stringPos <= end = [ClassName "barre"]
  | otherwise = []

barreOnFret :: Int         -- String position
            -> Int         -- Fret position
            -> Maybe Barre -- Barre
            -> Maybe Int
barreOnFret stringPos fretPos (Just barre@(Barre barreFretPos first last)) =
  if barreFretPos == fretPos && stringPos >= first && stringPos <= last
    then (Just barreFretPos)
    else Nothing
barreOnFret _ _ _ = Nothing

renderFret :: forall p i.
              Int         -- String position
           -> Pos         -- Root note on string
           -> Int         -- Fret position to render
           -> Finger      -- Finger on this string to render
           -> Maybe Barre -- Barre
           -> Maybe (HH.HTML p i)
renderFret stringPos rootPos fretPos fing barre =
  case barreOnFret stringPos fretPos barre of
    Just barreFretPos ->
      let barreClasses = maybe [] (barreClassNames stringPos) barre
          text = case fing of
                   B n -> getNoteName (posToNote (step n rootPos))
                   _ -> ""
      in Just $ HH.span
          [ HP.classes ([ClassName "circle"] <> barreClasses) ]
          [ HH.span
              [ HP.classes [ClassName "circle-info"] ]
              [ HH.text text ]
          ]
    _ ->
      if (getFingerPos fing) == fretPos
        then
          let text = case fing of
                      B n -> getNoteName (posToNote (step n rootPos))
                      F n -> getNoteName (posToNote (step n rootPos))
                      X -> "X"
          in Just $ HH.span
              [ HP.classes [ClassName "circle"] ]
              [ HH.span
                  [ HP.classes [ClassName "circle-info"] ]
                  [ HH.text text ]
              ]
        else Nothing

renderCircle :: forall p i.
                Maybe Barre
             -> Int    -- Note pos
             -> Int    -- String position
             -> String -- Text to display
             -> HH.HTML p i
renderCircle barre pos stringPos s = 
  let barreClass = maybe [] (barreClassNames stringPos) barre
  in HH.span
      [ HP.classes ([ClassName "circle"] <> barreClass) ]
      [ HH.span
          [ HP.classes [ClassName "circle-info"] ]
          [ HH.text s ]
      ]

renderChordInfo :: forall p i. State -> HH.HTML p i
renderChordInfo s =
  let htmls = case s of
                NoChord -> []
                (Chord p q i _) -> Com.chordHtml (posToNote p) q i
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
                B n -> max m n
                F n -> max m n)
      0
      fs) + 1)

-- Draw a string on the instrument, drawing the frets of each string.
renderString :: forall p i.
                State
             -> Pos -- Root note of string
             -> Int -- n-th string (0 is the left-most string)
             -> HH.HTML p i
renderString s rootPos stringPos =
  let fing = case s of
                   NoChord -> X
                   Chord p q i (Fingering _ fs) -> fromMaybe X (index fs stringPos)
      barre = case s of
                NoChord -> Nothing
                Chord p q i f -> getBarre f
  in HH.span [ HP.classes [ClassName "string"] ]
       (renderFrets stringPos rootPos (numFretsToRender s) fing barre)

-- Draw the frets of a string.
renderFrets :: forall p i.
               Int         -- String position
            -> Pos         -- Root note on string
            -> Int         -- Number of frets to render
            -> Finger      -- Finger to be played for this fret on this string
            -> Maybe Barre
            -> Array (HH.HTML p i)
renderFrets stringPos rootPos numFrets fing barre =
  foldl
    (\htmls fretPos ->
      snoc htmls
            (HH.span
              [ HP.classes [ClassName "fret"] ]
              (maybe [] (\h -> [h]) (renderFret stringPos rootPos fretPos fing barre))))
    []
    (range 0 (numFrets - 1))

fretboardComponent :: forall m. H.Component HH.HTML Query Input Message m
fretboardComponent =
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
      , renderString state 7 0 -- G = 7
      , renderString state 0 1 -- C = 0
      , renderString state 4 2 -- E = 4
      , renderString state 9 3 -- A = 9
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    ChordChange p q i next -> do
      -- state <- H.get
      -- let nextState = not state
      let s = case findUkeChord p q i of
                 Just fingering -> (Chord p q i fingering)
                 _ -> NoChord
      H.put s
      -- H.raise $ Toggled nextState
      pure next
    ClearChord next -> do
      H.put NoChord
      pure next
    IsOn reply -> do
      -- state <- H.get
      pure (reply true)

  initialState :: Input -> State
  initialState input =
    case input of
      NoChordInput -> NoChord
      ChordInput p q i ->
        case findUkeChord p q i of
          Just fingering -> (Chord p q i fingering)
          _ -> NoChord
  
  -- This component receives an Input from the parent component
  receiver :: Input -> Maybe (Query Unit)
  receiver input =
    case input of
      NoChordInput -> Just (ClearChord unit)
      ChordInput p q i -> Just (ChordChange p q i unit)