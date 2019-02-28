module Component.App where

import Prelude
import Chords as C

import Component.ChordSelector as CS
import Component.Fretboard as FB
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data State
  = NoChord
  | Chord C.Pos C.ChordQuality C.ChordInterval

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
  initialState =
    case CS.initialState of
      CS.Chord (Just p) (Just q) (Just i) -> Chord p q i
      _ -> NoChord

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state =
    let fbState = case state of
                    NoChord -> FB.NoChordInput
                    Chord p q i -> FB.ChordInput p q i
    in HH.div_
        [ HH.slot' CP.cp1 unit CS.component unit (HE.input HandleChordSelector)
        , HH.slot' CP.cp2 unit FB.component fbState (HE.input HandleFretboard)
        ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    HandleChordSelector (CS.ChordSelected pos quality interval) next -> do
      -- clear <- H.query' CP.cp1 unit (H.request CS.Clear)
      -- H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })
      H.put (Chord pos quality interval)
      pure next
    HandleChordSelector CS.NoMessage next -> do
      pure next
    HandleFretboard m next -> do
      pure next