module Component.App where

-- TODO refactor and comment everything throughly!

import Prelude

import Component.ChordSelector as CS
import Component.Fretboard as FB
import Component.Fretboards as FBS
import Data.Array as A
import Data.Either.Nested (Either3)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model as M

type State = { chord :: M.Chord, chords :: Array M.Chord }

data Query a
  = HandleChordSelector CS.Message a
  | HandleFretboard FB.Message a
  | HandleFretboards FBS.Message a
  | AddChord a

type ChildQuery = Coproduct3 CS.Query FB.Query FBS.Query

type ChildSlot = Either3 Unit Unit Unit

newtype FretboardSlot = FretboardSlot Int

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
  initialState = { chord: M.initialChord, chords: [] }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state =
    HH.div
      [ HP.classes [ClassName "main-component"] ]
      [ HH.div
          [ HP.classes [ClassName "top-component"] ]
          [ HH.div
              [ HP.classes [ClassName "chord-selector-section"] ]

              -- Render the ChordSelector component. It doesn't have any inputs to it, so pass in
              -- unit as the input. It emits a message whenever a valid chord is selected, which is
              -- passed via the HandleChordSelector wrapper.
              [ HH.slot' CP.cp1 unit CS.component unit (HE.input HandleChordSelector)

              -- Render the [Add] button.
              , HH.div
                  [ HP.classes [ ClassName "selection", ClassName "wide", ClassName "btn", ClassName "clickable" ]
                  , HE.onClick (HE.input_ AddChord) ]
                  [ HH.text "Add" ]
              ]

            -- Render the current fretboard.
            , HH.div
                [ HP.classes [ ClassName "fretboard-active" ] ]
                [ HH.slot' CP.cp2 unit FB.component (FB.ChordInput state.chord) (HE.input HandleFretboard) ]
          ]

      -- Render all the fretboards. Passes in the list of chords to render as input to the
      -- fretboard.
      , HH.slot' CP.cp3 unit FBS.component (FBS.FretboardChords state.chords) (HE.input HandleFretboards)
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    HandleChordSelector (CS.ChordSelected chord) next -> do
      H.modify_ (_ { chord = chord })
      pure next
    HandleChordSelector CS.NoMessage next -> do
      pure next
    HandleFretboard m next -> do
      pure next
    HandleFretboards (FBS.NotifyRemove fbId) next -> do
      -- Remove by index
      H.modify_ (\s -> s { chords = foldlWithIndex (\i acc c -> if fbId == i then acc else A.snoc acc c) [] s.chords } )
      pure next
    AddChord next -> do
      -- Add the "active" chord into the list of archived chords.
      H.modify_ (\st -> st { chords = A.snoc st.chords st.chord })
      pure next