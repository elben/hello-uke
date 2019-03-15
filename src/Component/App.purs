module Component.App where

import Prelude

import Chords as C
import Model as M
import Component.ChordSelector as CS
import Component.Fretboard as FB
import Component.Fretboards as FBS
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..), maybe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { chord :: Maybe M.Chord }

data Query a
  = HandleChordSelector CS.Message a
  | HandleFretboard FB.Message a
  | HandleFretboards FBS.Message a
  | AddChord (Maybe M.Chord) a

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
      CS.Chord (Just n) (Just q) (Just i) -> { chord: Just (M.Chord n q i) }
      _ -> { chord: Nothing }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state =
    let fbInput = maybe FB.NoChordInput FB.ChordInput state.chord
    in HH.div_
        [ HH.slot' CP.cp1 unit CS.component unit (HE.input HandleChordSelector)
        , HH.div
            [ HP.classes [ClassName "selection", ClassName "wide", ClassName "btn"]
            , HE.onClick (HE.input_ (AddChord state.chord)) ]
            [ HH.text "Add" ]

        -- TODO what if we made the App state include ALL the chords, including the current one.
        -- We pass the list of ALL chords to FBS component, as the input. FBS basically takes as
        -- input only one thing: the list of chords to render. FBS is then "dumb" and just renders.
        , HH.slot' CP.cp2 unit FBS.component (FBS.ModifyActiveFretboardInput fbInput) (HE.input HandleFretboards)
        -- , HH.slot' CP.cp2 unit FBS.component (FBS.AddFretboardInput fbInput) (HE.input HandleFretboards)
        ] -- <> fretboards)

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    HandleChordSelector (CS.ChordSelected note quality interval) next -> do
      -- clear <- H.query' CP.cp1 unit (H.request CS.Clear)
      -- H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })
      H.put { chord: Just (M.Chord note quality interval) }
      pure next
    HandleChordSelector CS.NoMessage next -> do
      pure next
    HandleFretboard m next -> do
      pure next
    HandleFretboards m next -> do
      pure next
    AddChord chord next -> do
      -- TODO
      pure next