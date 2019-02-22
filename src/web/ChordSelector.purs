module ChordSelector where

import Chords
import Engine
import Prelude

import Data.Array (filter, range, snoc)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), foldl, index, intercalate, (:))
import Data.List.Lazy (replicate)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Engine (f, posToNote, step)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (i)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.EventPhase (EventPhase(..))
import Web.HTML.Event.EventTypes (offline)

data State = Chord (Maybe Pos) (Maybe ChordQuality) (Maybe ChordInterval)

data Query a
  = Clear a
  | SelectPos (Pos -> a)
  | SelectChordQuality (ChordQuality -> a)
  | SelectChordInterval (ChordInterval -> a)

type Input = Unit

data Message = Toggled Boolean

chordSelectorComponent :: forall m. H.Component HH.HTML Query Input Message m
chordSelectorComponent =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = Chord (Just 0) (Just Major) Nothing

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
                in filter (\q -> M.member q intervalsMap) chordIntervals

            -- Return all of them
            _ -> chordIntervals
    in
      HH.div
        [ HP.classes [ClassName "chord-selector"] ]
        [ HH.div
            [ HP.classes [ClassName "selector-section root-note-selector"] ]
            (map
                (\(Note name pos) -> HH.div [ HP.classes [ClassName "selection", ClassName "root-note-selection", ClassName "btn" ] ] [ HH.text name ])
                selectableNotes)
        , HH.div
            [ HP.classes [ClassName "selector-section chord-quality-selector"] ]
            (map
                (\q -> HH.div [ HP.classes [ClassName "selection", ClassName "chord-quality-selection", ClassName "btn" ] ] [ HH.text (show q) ])
                selectableChordQualities)
        , HH.div
            [ HP.classes [ClassName "selector-section chord-interval-selector"] ]
            -- TODO only show for the ones the selected root note has.
            (map
                (\i -> HH.div [ HP.classes [ClassName "selection", ClassName "chord-interval-selection", ClassName "btn" ] ] [ HH.text (humanChordInterval i) ])
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
    SelectPos reply -> do
      state <- H.get
      pure (reply 0)
    SelectChordQuality reply -> do
      state <- H.get
      pure (reply Major)
    SelectChordInterval reply -> do
      state <- H.get
      pure (reply Dom7)