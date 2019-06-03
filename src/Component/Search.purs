module Component.Search where

import Prelude

import Chords (Chord)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Parser (compute)

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
         , HE.onValueInput (HE.input QueryStringChanged)]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval qs = case qs of
    QueryStringChanged q next -> do
      let chords = compute q
      H.modify_ (_ {qs = q, chords = chords })
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