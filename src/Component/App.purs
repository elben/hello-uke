module Component.App where

import Prelude

import Chords (Chord, ChordInterval(..), ChordQuality(..))
import Component.ChordSelector as CS
import Component.Fretboard as FB
import Component.Fretboards as FBS
import Component.Search as S
import Data.Array as A
import Data.Either.Nested (Either4)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Maybe (Maybe(..), maybe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Notes as N

-- The state of the app.
--
-- * chord  - the active chord selected.
-- * chords - the list of chords added. Both the App component and the Fretboards component need to
--            sync this list of saved chords. Perhaps there's a way where only Fretboards needs to
--            know, but I haven't found a way. The reason is that in the render function of the App
--            component, I must pass in an Input to the Fretboards component. I also only have
--            access to the current State of App. So how do I say: the user hit "Add" for this
--            chord, please inject this into Fretboards' chords list? Other than using some
--            temporary variable in App's state? This means that when the user deletes a chord, we
--            need to update both App's and Fretboards' list of chords.
type State = { chord :: Chord, chords :: Array Chord }

-- Queries App can make.
--
-- * HandleChordSelector, HandleFretboard, HandleFretboards are wrappers around the messages that
--   these components can raise.
-- * AddChord is triggered when the user hits the [Add] button.
--
data Query a
  = HandleChordSelector CS.Message a
  | HandleFretboard FB.Message a
  | HandleFretboards FBS.Message a
  | HandleSearch S.Message a
  | AddChord a

-- Halogen requires a coproduct type of all the queriers a component's children can make.
type ChildQuery = Coproduct4 CS.Query FB.Query FBS.Query S.Query

-- A slot for each child component.
type ChildSlot = Either4 Unit Unit Unit Unit

initialChord :: Chord
initialChord = { note: N.c, quality: Major, interval: Triad }

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
  initialState = { chord: initialChord, chords: [] }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state =
    let chordAlreadyAdded = maybe false (const true) (A.findIndex ((==) state.chord) state.chords)
        addBtnClasses = 
          [ ClassName "selection"
          , ClassName "wide"
          , ClassName "btn"
          , ClassName (if chordAlreadyAdded then "not-clickable" else "clickable")
          , ClassName "add-chord"
          , ClassName (if chordAlreadyAdded then "already-added" else "")
          ]
        onClickProp = if chordAlreadyAdded then [] else [ HE.onClick (HE.input_ AddChord) ]
    in
      HH.div
        [ HP.classes [ClassName "main-component"] ]
        [ HH.div
            [ HP.classes [ClassName "top-component"] ]
            [ HH.div
                [ HP.classes [ClassName "chord-selector-section"] ]

                [
                -- Render the Search component
                  HH.slot' CP.cp4 unit S.component "" (HE.input HandleSearch)

                -- Render the ChordSelector component. It doesn't have any inputs to it, so pass in
                -- unit as the input. It emits a message whenever a valid chord is selected, which is
                -- passed via the HandleChordSelector wrapper.
                , HH.slot' CP.cp1 unit CS.component unit (HE.input HandleChordSelector)

                -- Render the [Add] button.
                , HH.div
                    (append [ HP.classes addBtnClasses ] onClickProp)
                    [ HH.text (if chordAlreadyAdded then "Already Added" else "Add") ]
                ]

              -- Render the current fretboard.
              , HH.div
                  [ HP.classes [ ClassName "fretboard-active" ] ]
                  [ HH.slot' CP.cp2 unit FB.component { chord: state.chord, displayActions: false } (HE.input HandleFretboard) ]
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
    HandleSearch (S.ChordSelectedMessage chord) next -> do
      H.modify_ (_ { chord = chord })
      pure next
    AddChord next -> do
      -- Add the "active" chord into the list of archived chords.
      H.modify_ (\st -> st { chords = A.snoc st.chords st.chord })
      pure next