module Component.App where

import Prelude

import Chords as C
import Component.ChordSelector as CS
import Component.Fretboard as FB
import Component.Fretboards as FBS
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Chord = Chord C.Note C.ChordQuality C.ChordInterval

type State = { chord :: Maybe Chord }

data Query a
  = HandleChordSelector CS.Message a
  | HandleFretboard FB.Message a
  | HandleFretboards FBS.Message a

type ChildQuery = Coproduct2 CS.Query FBS.Query

type ChildSlot = Either2 Unit Unit

newtype FretboardSlot = FretboardSlot Int

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
  initialState =
    case CS.initialState of
      CS.Chord (Just n) (Just q) (Just i) -> { chord: Just (Chord n q i) }
      _ -> { chord: Nothing }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state =
    let fbInput = maybe FB.NoChordInput (\(Chord n q i) -> FB.ChordInput n q i) state.chord
    in HH.div_
        [ HH.slot' CP.cp1 unit CS.component unit (HE.input HandleChordSelector)
        , HH.slot' CP.cp2 unit FBS.component (FBS.ModifyActiveFretboardInput fbInput) (HE.input HandleFretboards)
        ] -- <> fretboards)

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    HandleChordSelector (CS.ChordSelected note quality interval) next -> do
      -- clear <- H.query' CP.cp1 unit (H.request CS.Clear)
      -- H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })
      H.put { chord: Just (Chord note quality interval) }
      pure next
    HandleChordSelector (CS.ChordAdded note quality interval) next -> do
      -- TODO
      pure next
    HandleChordSelector CS.NoMessage next -> do
      pure next
    HandleFretboard m next -> do
      pure next
    HandleFretboards m next -> do
      pure next