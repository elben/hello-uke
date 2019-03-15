module Component.App where

import Prelude

import Chords as C
import Component.ChordSelector as CS
import Component.Fretboard as FB
import Component.Fretboards as FBS
import Data.Array as A
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..), maybe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model as M

type State = { chord :: Maybe M.Chord, chords :: Array M.Chord }

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
  initialState =
    case CS.initialState of
      CS.Chord (Just n) (Just q) (Just i) -> { chord: Just (M.Chord n q i), chords: [] }
      _ -> { chord: Nothing , chords: [] }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state =
    let fbInput = maybe FB.NoChordInput FB.ChordInput state.chord
    in HH.div_
        [ HH.slot' CP.cp1 unit CS.component unit (HE.input HandleChordSelector)
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
        , HH.slot' CP.cp2 unit FBS.component (FBS.FretboardChords ((maybe [] (\c -> [c]) state.chord) <> state.chords)) (HE.input HandleFretboards)
        -- , HH.slot' CP.cp2 unit FBS.component (FBS.AddFretboardInput fbInput) (HE.input HandleFretboards)
        ] -- <> fretboards)

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    HandleChordSelector (CS.ChordSelected note quality interval) next -> do
      -- clear <- H.query' CP.cp1 unit (H.request CS.Clear)
      -- H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })
      H.put { chord: Just (M.Chord note quality interval), chords: [] }
      pure next
    HandleChordSelector CS.NoMessage next -> do
      pure next
    HandleFretboard m next -> do
      pure next
    HandleFretboards m next -> do
      pure next
    AddChord next -> do
      s <- H.get
      H.put { chord: s.chord, chords: maybe s.chords (\c -> A.snoc s.chords c) s.chord }
      pure next