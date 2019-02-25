module Component.App where

import Prelude

import Component.ChordSelector as CS
import Component.Fretboard as FB
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data State = NoState

data Query a
  = HandleChordSelector CS.Message a
  | HandleFretboard FB.Message a

type ChildQuery = Coproduct2 CS.Query FB.Query

type ChildSlot = Either2 Unit Unit

-- derive instance eqChildSlot :: Eq ChildSlot
-- derive instance ordChildSlot :: Eq ChildSlot

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = NoState

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state = HH.div_
      [ HH.slot' CP.cp1 unit CS.chordSelectorComponent unit (HE.input HandleChordSelector)
  -- TODO how do I pass in the chord-selected into fretboard?
      , HH.slot' CP.cp2 unit FB.fretboardComponent FB.NoChordInput (HE.input HandleFretboard)
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    HandleChordSelector (CS.ChordSelected pos quality interval) next -> do
      -- clear <- H.query' CP.cp1 unit (H.request CS.Clear)
      -- H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })
      pure next
    HandleChordSelector CS.NoMessage next -> do
      pure next
    HandleFretboard m next -> do
      pure next