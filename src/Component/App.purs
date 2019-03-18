module Component.App where

-- TODO refactor and comment everything throughly!

import Prelude

import Component.ChordSelector as CS
import Component.Fretboard as FB
import Component.Fretboards as FBS
import Data.Array as A
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
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
  initialState = { chord: M.initialChord, chords: [] }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state =
    HH.div_
    -- Render the ChordSelector component. It doesn't have any inputs to it, so pass in
    -- unit as the input. It emits a message whenever a valid chord is selected, which is
    -- passed via the HandleChordSelector wrapper.
    [ HH.slot' CP.cp1 unit CS.component unit (HE.input HandleChordSelector)

    -- Render the [Add] button.
    , HH.div
        [ HP.classes [ClassName "selection", ClassName "wide", ClassName "btn"]
        , HE.onClick (HE.input_ AddChord) ]
        [ HH.text "Add" ]

    -- TODO what if we made the App state include ALL the chords, including the current one.
    -- We pass the list of ALL chords to FBS component, as the input. FBS basically takes as
    -- input only one thing: the list of chords to render. FBS is then "dumb" and just renders.
    --
    -- But is this good design? The App component doesn't need to know all the chords we have
    -- added. The Fretboards component should know that. When the App component gets an Add,
    -- it needs to somehow send a message to Fretboards to update, via the Input mechanism.
    -- But how? The Input to Fretboards is done in render, in which we only can query the state.
    --
    -- In the TODO example, see NewTask query
    -- https://github.com/slamdata/purescript-halogen/blob/v4.0.0/examples/todo/src/Component/List.purs
    -- When we click "Add" here, we could pass in the CURRENT chord from the ChordSelector.
    -- But again, we can only modify the state.
    --
    -- Maybe we SHOULD just pass in all the chords into Fretboards...
    , HH.slot' CP.cp2 unit FBS.component (FBS.FretboardChords (A.cons state.chord state.chords)) (HE.input HandleFretboards)
    -- , HH.slot' CP.cp2 unit FBS.component (FBS.AddFretboardInput fbInput) (HE.input HandleFretboards)
    ] -- <> fretboards)

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    HandleChordSelector (CS.ChordSelected note quality interval) next -> do
      -- st <- H.get
      -- H.put ({ chord: M.Chord note quality interval, chords: st.chords })
      H.modify_ (_ { chord = M.Chord note quality interval })
      pure next
    HandleChordSelector CS.NoMessage next -> do
      pure next
    HandleFretboard m next -> do
      pure next
    HandleFretboards m next -> do
      pure next
    AddChord next -> do
      -- st <- H.get
      -- H.put ({ chord: st.chord, chords: A.snoc st.chords st.chord })
      H.modify_ (\st -> st { chords = A.snoc st.chords st.chord })
      pure next