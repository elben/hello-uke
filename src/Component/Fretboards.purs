module Component.Fretboards where

import Model
import Prelude

import Chords as C
import Component.Fretboard as FB
import Data.Array (mapWithIndex)
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { fretboards :: Array FretboardState
  }

type FretboardState =
  { chord :: Chord
  , id :: FretboardId }

data Query a
  = HandleFretboardMessage FretboardId FB.Message a
  | ReplaceChords (Array Chord) a

data Input
  = FretboardChords (Array Chord)

newtype FretboardSlot = FretboardSlot Int
derive instance eqFretboardSlot :: Eq FretboardSlot
derive instance ordFretboardSlot :: Ord FretboardSlot

data Message = Toggled Boolean

-- component :: forall m. Applicative m => H.Component HH.HTML Query Input Message m
component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState: initialState
    , render
    , eval
    , receiver: receiver
    }

  where

  initialState :: Input -> State
  initialState (FretboardChords chords) = { fretboards: mapWithIndex (\i c -> { chord: c, id: i } ) chords }

  render :: State -> H.ParentHTML Query FB.Query FretboardSlot m
  render s = HH.div [ HP.classes [ ClassName "fretboards" ] ] (map renderFretboard s.fretboards)

  renderFretboard :: FretboardState -> H.ParentHTML Query FB.Query FretboardSlot m
  renderFretboard fbState =
    HH.slot (FretboardSlot fbState.id) FB.component (FB.ChordInput fbState.chord) (HE.input (HandleFretboardMessage fbState.id))

  eval :: Query ~> H.ParentDSL State Query FB.Query FretboardSlot Message m
  eval (HandleFretboardMessage fbId msg next) = pure next
  eval (ReplaceChords chords next) = do
    H.modify_ (_ { fretboards = mapWithIndex (\i c -> { chord: c, id: i }) chords } )
    pure next

  receiver :: Input -> Maybe (Query Unit)
  receiver (FretboardChords chords) = Just (ReplaceChords chords unit)