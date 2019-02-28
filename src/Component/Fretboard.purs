module Component.Fretboard where

import Chords
import Prelude

import Component.Common as Com
import Data.Array (index, range, snoc)
import Data.List (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
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

renderCircle :: forall p i. String -> HH.HTML p i
renderCircle s = 
  HH.span
    [ HP.classes [ClassName "circle"] ]
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
numFretsToRender (Chord p q i fs) =
  -- Draw at least 4 frets, including the open string fret (the one behind the nut)
  max 4
    ((foldl
      (\m f -> case f of
                X -> m
                Finger n -> max m n)
      0
      fs) + 1)

-- Draw a string on the instrument, drawing the frets of each string.
renderString :: forall p i. State -> Pos -> Int -> HH.HTML p i
renderString s baseNote n =
  let fing = case s of
                   NoChord -> X
                   Chord p q i fs -> fromMaybe X (index fs n)
  in HH.span [ HP.classes [ClassName "string"] ]
       (renderFrets baseNote (numFretsToRender s) fing)

-- Draw the frets of a string.
renderFrets :: forall p i. Pos -> Int -> Finger -> Array (HH.HTML p i)
renderFrets baseNote numFrets f =
  let circle = case f of
                  X -> renderCircle "X"
                  -- Just choose the first note name for now (e.g. C# instead of Db)
                  Finger n -> renderCircle (getNoteName (posToNote (step n baseNote)))
      fingerIdx = case f of
                    X -> 0
                    Finger n -> n
  in
    foldl
      (\htmls idx ->
        -- If the idx-th fret is where the finger is to be played, then
        -- draw the circle representing the finger.
        let inside = if idx == fingerIdx then [circle] else []
        in snoc htmls (HH.span [ HP.classes [ClassName "fret"] ] inside)
      )
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