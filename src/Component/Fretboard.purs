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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Notes (Note(..), Accidental(..), Pos, defaultAccidental, findNoteForAccidental, humanNote)

type State = { chord :: Chord
             , fingering :: Fingering
             , displayActions :: Boolean
             }

humanChord :: State -> String
humanChord s = humanNote s.chord.note <> humanChordMod s.chord.quality s.chord.interval

data Query a
  = ChordChange Chord a
  | RemoveChord a
  | IsOn (Boolean -> a)

type Input = { chord :: Chord
            -- Whether the actions bar should be displayed or not.
             , displayActions :: Boolean }

data Message = NotifyRemove

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
  if not (isBarreOnFret || (getFingerPos fing) == fretPos || (fingNoPlay fing && fretPos == 0))
    then Nothing -- Neither a fret nor a finger is put in this string/fret position nor it's an unplayed string.
    else
      let classes = if isBarreOnFret
                    then maybe [] (barreClassNames stringPos) barre
                    else
                      if (fingNoPlay fing && fretPos == 0)
                      then [ ClassName "no-play" ]
                      else []
          text = case fing of
                   -- If fret in (stringPos, fretPos) is an unplayed barre, don't show note
                   -- because another finger (F n) will be playing this string instead.
                   F n -> if isBarreOnFret && higherFingerPlaying barre n
                            then Nothing
                            else map humanNote (findNoteForAccidental (step n rootPos) acc)

                   X -> Just "✖︎"
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
  let htmls = Com.chordHtml s.chord.note s.chord.quality s.chord.interval
  in HH.div
       [ HP.classes [ ClassName "chord-meta-item", ClassName "chord-info" ] ]
       htmls

-- Render the action bar (e.g. the close icon). Can trigger a RemoveChord query.
renderChordActions :: forall p. HH.HTML p (Query Unit)
renderChordActions =
  HH.div
    [ HP.classes [ ClassName "chord-actions" ] ]
    [ HH.span
        [ HP.classes [ ClassName "chord-action-delete", ClassName "clickable" ]
        , HE.onClick (HE.input_ RemoveChord) ]
        [ HH.text "✖︎" ]
    ]

-- Determine the number of frets to draw for this state. Draw at least four frets (including the one
-- behind the nut).
numFretsToRender :: State -> Int
numFretsToRender s =
  -- Draw at least 4 frets, including the open string fret (the one behind the nut)
  max 4
    ((foldl
      (\m f -> case f of
                X -> m
                F n -> max m n)
      0
      (getFingers s.fingering)) + 1)

-- Draw a string on the instrument, drawing the frets of each string.
renderString :: forall p i.
                State
             -> Int -- n-th string (0 is the left-most string)
             -> Pos -- Root note of string
             -> HH.HTML p i
renderString state stringPos rootPos =
  let fing  = fromMaybe X (index (getFingers state.fingering) stringPos)
      barre = getBarre state.fingering
      acc   = let Note _ a p = state.chord.note
              in if a == Natural then defaultAccidental p else a
  in HH.span [ HP.classes [ClassName "string"] ]
       (renderFrets stringPos rootPos (numFretsToRender state) acc fing barre)

-- Potentially-render fret markers, given the string and fret position. The frets are rendered
-- for the middle string. On the 12th fret, the two side strings get the markers.
renderFretMarker :: forall p i.
                    Int -- n-th String
                 -> Pos -- Fret number
                 -> Maybe (HH.HTML p i)
renderFretMarker stringPos fretPos =
  if (stringPos == 1 && (fretPos == 5 || fretPos == 7 || fretPos == 10 || fretPos == 15))
     || ((stringPos == 0 || stringPos == 2) && fretPos == 12)
  then Just $ HH.span [ HP.classes [ClassName "fret-marker"]
                      , HP.title (ordinalize fretPos <> " fret") ]
                      [ HH.text "◉" ]
  else Nothing

ordinalize :: Int -> String
ordinalize n | n == 1 || n == 21 = show n <> "st"
             | n == 2 || n == 22 = show n <> "nd"
             | n == 3 || n == 23 = show n <> "rd"
             | otherwise = show n <> "th"

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
              (  (maybe [] (\h -> [h]) (renderFretMarker stringPos fretPos))
              <> (maybe [] (\h -> [h]) (renderFret stringPos rootPos fretPos acc fing barre)))))
    []
    (range 0 (numFrets - 1))

-- Finds the fingering for a chord.
chordFingering :: Chord -> Fingering
chordFingering chord =
    let Note _ _ pos = chord.note
    in findUkeFingering pos chord.quality chord.interval

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
    let
      actionBar =
        if state.displayActions
          then [renderChordActions]
          else []
    in
      HH.div
        [ HP.classes [ClassName "fretboard"] ]

        (actionBar <>
        [ HH.div
            [ HP.classes [ ClassName "chord-meta" ] ]
            [ HH.div [ HP.classes [ ClassName "chord-meta-item" ] ] []
            , renderChordInfo state
            ]
        , renderString state 0 7 -- G
        , renderString state 1 0 -- C
        , renderString state 2 4 -- E
        , renderString state 3 9 -- A
        ])

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval q = case q of
    ChordChange chord next -> do
      H.modify_  (_ { chord = chord
                    , fingering = chordFingering chord })
      pure next
    RemoveChord next -> do
      s <- H.get
      -- https://github.com/slamdata/purescript-halogen/blob/v4.0.0/examples/todo/src/Component/List.purs
      H.raise NotifyRemove
      pure next
    IsOn reply -> do
      pure (reply true)

  initialState :: Input -> State
  initialState input = { chord: input.chord
                       , displayActions: input.displayActions
                       , fingering: chordFingering input.chord
                       }
  
  -- This component receives an Input from the parent component
  receiver :: Input -> Maybe (Query Unit)
  receiver input = Just (ChordChange input.chord unit)