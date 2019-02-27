module Component.ChordSelector where

import Chords
import Prelude

import Data.Array (filter, snoc)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Engine (allNotes)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data State = Chord (Maybe Pos) (Maybe ChordQuality) (Maybe ChordInterval)

getStatePos :: State -> Maybe Pos
getStatePos (Chord p _ _) = p

getStateChordQuality :: State -> Maybe ChordQuality
getStateChordQuality (Chord _ q _) = q

getStateChordInterval :: State -> Maybe ChordInterval
getStateChordInterval (Chord _ _ i) = i

setStatePos :: Pos -> State -> State
setStatePos pos (Chord _ q i) = Chord (Just pos) q i

setStateChordQuality :: ChordQuality -> State -> State
setStateChordQuality q (Chord p _ i) = Chord p (Just q) i

setStateChordInterval :: ChordInterval -> State -> State
setStateChordInterval i (Chord p q _) = Chord p q (Just i)

isChordSelected :: State -> Boolean
isChordSelected (Chord (Just _) (Just _) (Just _)) = true
isChordSelected _ = false

toMessage :: State -> Message
toMessage (Chord (Just p) (Just q) (Just i)) = ChordSelected p q i
toMessage _ = NoMessage

data Query a
  -- These are "actions"
  -- https://pursuit.purescript.org/packages/purescript-halogen/3.1.3/docs/Halogen.Query#t:Action
  = Clear a
  | SelectPos Pos a
  | SelectChordQuality ChordQuality a
  | SelectChordInterval ChordInterval a

type Input = Unit

data Message =
  NoMessage
  | ChordSelected Pos ChordQuality ChordInterval

rootNoteSelectorClasses :: Array ClassName
rootNoteSelectorClasses = [ClassName "selection", ClassName "root-note-selection", ClassName "btn" ]

chordQualitySelectorClasses :: Array ClassName
chordQualitySelectorClasses = [ClassName "selection", ClassName "chord-quality-selection", ClassName "btn" ]

chordIntervalSelectorClasses :: Array ClassName
chordIntervalSelectorClasses = [ClassName "selection", ClassName "chord-interval-selection", ClassName "btn" ]

initialState :: State
initialState = Chord (Just 0) (Just Major) (Just Triad)

chordSelectorComponent :: forall m. H.Component HH.HTML Query Input Message m
chordSelectorComponent =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    let selectableNotes = filter (\(Note _ pos) -> M.member pos ukeChords) allNotes
        selectableChordQualities =
          case state of
            -- Filter to the ones available for this position. Go through chordQualities, filtering
            -- each one by the big map, to produce consistent ordering.
            Chord (Just pos) _ _ ->
                let qualitiesMap = fromMaybe M.empty (M.lookup pos ukeChords)
                in filter (\q -> M.member q qualitiesMap) chordQualities

            -- Return all of them
            _ -> chordQualities
        selectableChordIntervals =
          case state of
            -- Filter to the ones available for this position
            Chord (Just pos) (Just q) _ ->
                let intervalsMap = fromMaybe M.empty (M.lookup pos ukeChords >>= M.lookup q)
                in filter (\qual -> M.member qual intervalsMap) chordIntervals

            -- Return all of them
            _ -> chordIntervals
    
        isPosSelected :: Pos -> Boolean
        isPosSelected p = maybe false ((==) p) (getStatePos state)

        isChordQualitySelected :: ChordQuality -> Boolean
        isChordQualitySelected q = maybe false ((==) q) (getStateChordQuality state)

        isChordIntervalSelected :: ChordInterval -> Boolean
        isChordIntervalSelected q = maybe false ((==) q) (getStateChordInterval state)
    in
      HH.div
        [ HP.classes [ClassName "chord-selector"] ]
        [ HH.div
            [ HP.classes [ClassName "selector-section", ClassName "root-note-selector"] ]
            (map
                (\(Note name pos) ->
                  let classes = if isPosSelected pos then snoc rootNoteSelectorClasses (ClassName "selected") else rootNoteSelectorClasses
                  in HH.div
                       [ HP.classes classes
                       , HE.onClick (HE.input_ (SelectPos pos)) ]
                       [ HH.text name ])
                selectableNotes)
        , HH.div
            [ HP.classes [ClassName "selector-section", ClassName "chord-quality-selector"] ]
            (map
                (\q ->
                  let classes = if isChordQualitySelected q then snoc chordQualitySelectorClasses (ClassName "selected") else chordQualitySelectorClasses
                  in HH.div
                       [ HP.classes classes
                       , HE.onClick (HE.input_ (SelectChordQuality q)) ]
                       [ HH.text (show q) ])
                selectableChordQualities)
        , HH.div
            [ HP.classes [ClassName "selector-section", ClassName "chord-interval-selector"] ]
            (map
                (\i ->
                  let classes = if isChordIntervalSelected i then snoc chordIntervalSelectorClasses (ClassName "selected") else chordIntervalSelectorClasses
                  in HH.div
                       [ HP.classes classes
                       , HE.onClick (HE.input_ (SelectChordInterval i)) ]
                       [ HH.text (humanChordInterval i) ])
                selectableChordIntervals)
        ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Clear next -> do
      state <- H.get
      -- let nextState = not state
      -- H.put nextState
      -- H.raise $ Toggled nextState
      pure next
    SelectPos pos next -> do
      state <- H.get
      let state' = setStatePos pos state
      H.put state'
      H.raise (toMessage state')
      pure next
    SelectChordQuality quality next -> do
      state <- H.get
      let state' = setStateChordQuality quality state
      H.put state'
      H.raise (toMessage state')
      pure next
    SelectChordInterval interval next -> do
      state <- H.get
      let state' = setStateChordInterval interval state
      H.put state'
      H.raise (toMessage state')
      pure next