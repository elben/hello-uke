module Component.ChordSelector where

import Prelude

import Chords (Chord, ChordInterval(..), ChordQuality(..), buildChord, chordIntervals, chordQualities, labelChordInterval, ukeChords)
import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Notes (Note(..), allNotes, humanNote)
import Notes as N

-- Carry Notes, not Pos. So that users can deleniate between A# and Bb.
data State = State (Maybe Note) (Maybe ChordQuality) (Maybe ChordInterval)

instance showState :: Show State where
  show (State n q i) = "State " <> show n <> " " <> show q <> " " <> show i

getStateNote :: State -> Maybe Note
getStateNote (State p _ _) = p

getStateChordQuality :: State -> Maybe ChordQuality
getStateChordQuality (State _ q _) = q

getStateChordInterval :: State -> Maybe ChordInterval
getStateChordInterval (State _ _ i) = i

setStateNote :: Note -> State -> State
setStateNote note (State _ q i) = State (Just note) q i

setStateChordQuality :: ChordQuality -> State -> State
setStateChordQuality q (State p _ i) = State p (Just q) i

setStateChordInterval :: ChordInterval -> State -> State
setStateChordInterval i (State p q _) = State p q (Just i)

isChordSelected :: State -> Boolean
isChordSelected (State (Just _) (Just _) (Just _)) = true
isChordSelected _ = false

toMessage :: (Chord -> Message) -> State -> Message
toMessage f (State (Just n) (Just q) (Just i)) = f (buildChord n q i)
toMessage _ _ = NoMessage

data Query a
  -- These are "actions"
  -- https://pursuit.purescript.org/packages/purescript-halogen/3.1.3/docs/Halogen.Query#t:Action
  = Clear a
  | SelectNote Note a
  | SelectChordQuality ChordQuality a
  | SelectChordInterval ChordInterval a
  | ChordSelectedQuery Chord a

data Input =
    NoInput
  | ChordSelectedInput Chord

data Message =
  -- A no-op to handle when a full-chord is not (yet) selected. In
  -- practice this doesn't happen, but we still need a way to signal a no-op.
    NoMessage
  | ChordSelected Chord

rootNoteSelectorClasses :: Array ClassName
rootNoteSelectorClasses = [ClassName "selection", ClassName "root-note-selection", ClassName "btn", ClassName "clickable" ]

chordQualitySelectorClasses :: Array ClassName
chordQualitySelectorClasses = [ClassName "selection", ClassName "chord-quality-selection", ClassName "btn", ClassName "clickable" ]

chordIntervalSelectorClasses :: Array ClassName
chordIntervalSelectorClasses = [ClassName "selection", ClassName "chord-interval-selection", ClassName "btn", ClassName "clickable" ]

selectableChordQualities :: State -> Array ChordQuality
selectableChordQualities state =
  case state of
    -- Filter to the ones available for this position. Go through chordQualities, filtering
    -- each one by the big map, to produce consistent ordering.
    State (Just (Note _ _ pos)) _ _ ->
        let qualitiesMap = fromMaybe Map.empty (Map.lookup pos ukeChords)
        in A.filter (\q -> Map.member q qualitiesMap) chordQualities

    -- Return all of them
    _ -> chordQualities

selectableChordIntervals :: State -> Array ChordInterval
selectableChordIntervals state =
  case state of
    -- Filter to the ones available for this position
    State (Just (Note _ _ pos)) (Just q) _ ->
        let intervalsMap = fromMaybe Map.empty (Map.lookup pos ukeChords >>= Map.lookup q)
        in A.filter (\i -> Map.member i intervalsMap) chordIntervals

    -- Return all of them
    _ -> chordIntervals

-- For a specific item in the state, figure out if we can use the *existing* selection
-- based off the list of available options, or use the given default value.
considerStateSelection :: forall a. (Eq a) =>
                          (State -> Maybe a)
                       -- ^ Gets the thing we are considering from the state
                       -> (State -> Array a)
                       -- ^ Gets the list of available options from the state
                       -> (a -> State -> State)
                       -- ^ Sets the thing into the state
                       -> a
                       -- ^ The default value to set to
                       -> State
                       -- ^ The state to modify
                       -> State
                       -- ^ The potentially-modified state
considerStateSelection get getSelection set default state =
  case get state of
    Just a ->
      let selection = getSelection state
      in
        if A.elem a selection
          -- The currently-set `a` in the state is available, so leave as-is.
          then state
          -- The currently-set `a` is not allowed; set to default value.
          else set (fromMaybe default (A.index selection 0)) state

    _ -> state


defaultInterval :: ChordQuality -> ChordInterval
defaultInterval Suspended = Second
defaultInterval _ = Triad

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver
    }
  where

  initialState :: Input -> State
  initialState NoInput = State (Just N.c) (Just Major) (Just Triad)
  initialState (ChordSelectedInput chord) = State (Just chord.note) (Just chord.quality) (Just chord.interval)

  receiver :: Input -> Maybe (Query Unit)
  receiver NoInput = Nothing
  receiver (ChordSelectedInput chord) = Just (ChordSelectedQuery chord unit)

  render :: State -> H.ComponentHTML Query
  render state =
    let selectableNotes = A.filter (\(Note _ _ pos) -> Map.member pos ukeChords) allNotes
    
        isNoteSelected :: Note -> Boolean
        isNoteSelected n = maybe false ((==) n) (getStateNote state)

        isChordQualitySelected :: ChordQuality -> Boolean
        isChordQualitySelected q = maybe false ((==) q) (getStateChordQuality state)

        isChordIntervalSelected :: ChordInterval -> Boolean
        isChordIntervalSelected q = maybe false ((==) q) (getStateChordInterval state)
    in
      HH.div
        [ HP.classes [ClassName "chord-selector"] ]
        [ HH.div_
            [ HH.h3 [] [ HH.text "Root Note" ] ]
        , HH.div
            [ HP.classes [ClassName "selector-section", ClassName "root-note-selector"] ]
            (map
                (\note ->
                  let classes = if isNoteSelected note
                                  then A.snoc rootNoteSelectorClasses (ClassName "selected")
                                  else rootNoteSelectorClasses
                  in HH.div
                       [ HP.classes classes
                       , HE.onClick (HE.input_ (SelectNote note)) ]
                       [ HH.text (humanNote note) ])
                selectableNotes)
        , HH.div_
            [ HH.h3 [] [ HH.text "Quality" ] ]
        , HH.div
            [ HP.classes [ClassName "selector-section", ClassName "chord-quality-selector"] ]
            (map
                (\q ->
                  let classes = if isChordQualitySelected q
                                  then A.snoc chordQualitySelectorClasses (ClassName "selected")
                                  else chordQualitySelectorClasses
                  in HH.div
                       [ HP.classes classes
                       , HE.onClick (HE.input_ (SelectChordQuality q)) ]
                       [ HH.text (show q) ])
                (selectableChordQualities state))
        , HH.div_
            [ HH.h3 [] [ HH.text "Interval" ] ]
        , HH.div
            [ HP.classes [ClassName "selector-section", ClassName "chord-interval-selector"] ]
            (map
                (\i ->
                  let classes = if isChordIntervalSelected i
                                  then A.snoc chordIntervalSelectorClasses (ClassName "selected")
                                  else chordIntervalSelectorClasses
                  in HH.div
                       [ HP.classes classes
                       , HE.onClick (HE.input_ (SelectChordInterval i)) ]
                       [ HH.text (labelChordInterval i) ])
                -- Don't show the Triad in the UI, as it is the "default" interval.
                -- We want intervals to act like a modification, so "no modification"
                -- equals the Triad interval.
                -- (A.filter ((/=) Triad) (selectableChordIntervals state)))
                (selectableChordIntervals state))
        ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Clear next -> do
      state <- H.get
      -- let nextState = not state
      -- H.put nextState
      -- H.raise $ Toggled nextState
      pure next
    SelectNote note next -> do
      s1 <- H.get
      let s2 = setStateNote note s1

      -- With the newly-chosen note, check to see if that note has the currently-selected
      -- quality available as a selection. If not, then use the first chord quality in that
      -- note's bank of chord qualities.
      let s3 = considerStateSelection getStateChordQuality selectableChordQualities setStateChordQuality Major s2

      -- With the newly-chosen note and quality, check to see if the currently-selected
      -- interval is available. If not, then use the first chord interval in that
      -- note quality's bank of chord intervals.
      let s4 = considerStateSelection getStateChordInterval selectableChordIntervals setStateChordInterval Triad s3

      H.put s4
      H.raise (toMessage ChordSelected s4)
      pure next

    SelectChordQuality quality next -> do
      s1 <- H.get
      let s2 = setStateChordQuality quality s1
      let s3 = considerStateSelection getStateChordInterval selectableChordIntervals setStateChordInterval Triad s2
      H.put s3
      H.raise (toMessage ChordSelected s3)
      pure next

    SelectChordInterval interval next -> do
      state <- H.get
      let state' = if maybe false (\i -> i == interval) (getStateChordInterval state)
                     -- The same interval was clicked, so "unselect" it by choosing the default
                     -- interval for the quality.
                     then setStateChordInterval (maybe Triad defaultInterval (getStateChordQuality state)) state
                     -- A different interval was clicked, so choose the selected one.
                     else setStateChordInterval interval state
      H.put state'
      H.raise (toMessage ChordSelected state')
      pure next

    ChordSelectedQuery chord next -> do
      H.put $ State (Just chord.note) (Just chord.quality) (Just chord.interval)
      pure next