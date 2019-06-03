module Component.Search where

import Debug.Trace
import Prelude

import Chords (Chord, humanChordMod)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Notes (getNoteName, humanNote)
import Parser (findChords)

type State = { qs :: String          -- Query string
             , chord :: Maybe Chord  -- Selected chord
             , hover :: Maybe Chord  -- Chord user is hovering over
             , chords :: Array Chord -- Selectable chords
             }

data Query a = QueryStringChanged String a

type Input = String

data Message = QueryString String

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
      [ HP.classes [ ClassName "search" ] ]
      [ HH.input
         [ HP.placeholder "Hit S or / to search for chords"
         , HP.autocomplete false
         , HP.spellcheck false
         -- "HP.input_ InputSearch" was removed because Safari doesn't follow
         -- our CSS for some reason. Even DuckDuckGo doesn't use type="search", so.
         , HE.onValueInput (HE.input QueryStringChanged)
         ]
      , HH.div
          [ HP.classes [ ClassName "search-results" ] ]
          (map
            (\c ->
              HH.div
                [ HP.classes [ ClassName "search-result" ] ]
                [ HH.text (humanNote c.note <> humanChordMod c.quality c.interval) ]
            )
            state.chords)
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval qs = case qs of
    QueryStringChanged q next -> do
      let chords = findChords 5 q
      H.modify_ (_ {qs = q, chords = (trace (show chords) \_ -> chords) })
      pure next

  initialState :: Input -> State
  initialState input = { qs: input
                       , chord: Nothing
                       , hover: Nothing
                       , chords: []
                       }
  
  -- This component receives an Input from the parent component
  receiver :: Input -> Maybe (Query Unit)
  receiver input = Nothing