module ChordSelector where

import Prelude

import Chords
import Engine

import Data.Array (range, snoc)
import Data.List (List(..), foldl, index, intercalate, (:))
import Data.List.Lazy (replicate)
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

data State = NoChord
           | Chord (Maybe Pos) (Maybe ChordQuality) (Maybe ChordInterval)

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
  initialState = NoChord

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ HP.classes [ClassName "chord-selector"] ]
      [ HH.div
          [ HP.classes [ClassName "selector-section root-note-selector"] ] 
          (map (\(Note name pos) -> HH.div [ HP.classes [ClassName "selection", ClassName "root-note-selection", ClassName "btn" ] ] [ HH.text name ] ) allNotes)
      , HH.div
          [ HP.classes [ClassName "selector-section chord-quality-selector"] ] 
          (map (\q -> HH.div [ HP.classes [ClassName "selection", ClassName "chord-quality-selection", ClassName "btn"] ] [ HH.text (show q) ] ) chordQualities)
      , HH.div
          [ HP.classes [ClassName "selector-section chord-interval-selector"] ] 
          (map (\i -> HH.div [ HP.classes [ClassName "selection", ClassName "chord-interval-selection btn", ClassName "btn" ] ] [ HH.text (humanChordInterval i) ] ) chordIntervals)
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