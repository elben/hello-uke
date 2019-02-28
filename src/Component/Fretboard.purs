module Component.Fretboard where

import Chords
import Prelude

import Component.Common as Com
import Data.Array (index, range, snoc)
import Data.List (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
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

-- TODO!! refactor this and its usage to simplify the whole Barre and fret position thing.
renderCircle :: forall p i.
                Maybe Barre
             -> Int    -- Note pos
             -> Int    -- String position
             -> String -- Text to display
             -> HH.HTML p i
renderCircle barre pos stringPos s = 
  let barreClass = case barre of
                     Just (Barre barreFret start end) ->
                       if pos == barreFret
                         then
                            if stringPos == start
                              -- First barred string
                              then [ClassName "barre", ClassName "first"]
                              else if stringPos == end
                                    -- Last barred string
                                    then [ClassName "barre", ClassName "last"]
                                    else if stringPos >= start && stringPos <= end
                                            -- Inside a barre
                                            then [ClassName "barre"]
                                            else []
                         else []
                     _ -> []
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
             -> Pos -- Base note of string
             -> Int -- n-th string (0 is the left-most string)
             -> HH.HTML p i
renderString s baseNote stringPos =
  let fing = case s of
                   NoChord -> X
                   Chord p q i (Fingering _ fs) -> fromMaybe X (index fs stringPos)
      barre = case s of
                NoChord -> Nothing
                Chord p q i f -> getBarre f
  in HH.span [ HP.classes [ClassName "string"] ]
       (renderFrets baseNote stringPos (numFretsToRender s) fing barre)

-- Draw the frets of a string.
renderFrets :: forall p i.
               Pos
            -> Int         -- Base note
            -> Int         -- String position
            -> Finger      -- Finger to be played for this fret on this string
            -> Maybe Barre
            -> Array (HH.HTML p i)
renderFrets baseNote stringPos numFrets f barre =
  let 
      fingerIdx = case f of
                    X -> 0
                    B n -> n
                    F n -> n
  in
    foldl
      (\htmls idx ->

        let barreHtml =
              case barre of
                Just (Barre barreFret first last) ->
                  if stringPos >= first && stringPos <= last
                    then
                      case f of
                        F n -> 
                          if idx == barreFret
                          -- A barre behind the finger for this string, so don't show note name.
                          then [renderCircle barre barreFret stringPos ""]
                          else []
                        B n ->
                          if idx == n
                          -- The fret on this string is barred, but the barred note is part of the
                          -- chord. So render the note name.
                          then [renderCircle barre n stringPos (getNoteName (posToNote (step n baseNote)))]
                          else []
                        _ -> []
                    else
                      []

                _ -> []

            noteHtml =
              if idx == fingerIdx
                then
                  case f of
                    F n -> [renderCircle barre n stringPos (getNoteName (posToNote (step n baseNote)))]
                    B n -> [renderCircle barre n stringPos (getNoteName (posToNote (step n baseNote)))]
                    X -> [renderCircle barre (-1) stringPos "X"]
                else []
            
            -- combined = fromMaybe [] (sequence [barreHtml, noteHtml])

        -- If the idx-th fret is where the finger is to be played, then
        -- draw the circle representing the finger.

        -- let circle = case f of
        --                X -> renderCircle barre (-1) stringPos "X"

        --                -- Just choose the first note name for now (e.g. C# instead of Db)
        --                B n -> renderCircle barre n stringPos (getNoteName (posToNote (step n baseNote)))

        --                -- Just choose the first note name for now (e.g. C# instead of Db)
        --                F n -> renderCircle barre n stringPos (getNoteName (posToNote (step n baseNote)))
        --     inside = if idx == fingerIdx then [circle] else []

        in snoc htmls (HH.span [ HP.classes [ClassName "fret"] ] (barreHtml <> noteHtml))
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