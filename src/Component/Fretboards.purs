module Component.Fretboards where

import Model
import Prelude

import Chords as C
import Component.Fretboard as FB
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { fretboards :: Array FretboardState
  , nextId :: Int
  }

type FretboardState =
  { input :: FB.Input
  , id :: FretboardId }

data Query a
  = HandleFretboardMessage FretboardId FB.Message a
  | AddFretboard FB.Input a
  | ModifyActiveFretboard FB.Input a

data Input
  = AddFretboardInput FB.Input
  | ModifyActiveFretboardInput FB.Input

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
  initialState (ModifyActiveFretboardInput fbInput) = { fretboards: [ { input: fbInput, id: 0 } ], nextId: 0 }
  initialState (AddFretboardInput fbInput) =          { fretboards: [ { input: fbInput, id: 0 } ], nextId: 0 }

  render :: State -> H.ParentHTML Query FB.Query FretboardSlot m
  render s = HH.div [ HP.classes [ ClassName "fretboards" ] ] (map renderFretboard s.fretboards)

  renderFretboard :: FretboardState -> H.ParentHTML Query FB.Query FretboardSlot m
  renderFretboard fbState =
    HH.slot (FretboardSlot fbState.id) FB.component fbState.input (HE.input (HandleFretboardMessage fbState.id))

  eval :: Query ~> H.ParentDSL State Query FB.Query FretboardSlot Message m
  eval (HandleFretboardMessage fbId msg next) = pure next
  eval (AddFretboard fbInput next) = do
    s <- H.get
    let s2 = case fbInput of
               FB.ChordInput _ -> do
                 -- TODO get the next ID
                 s { fretboards = A.snoc s.fretboards { input: fbInput, id: 1 } }
               FB.NoChordInput -> s
    H.put s2
    pure next
  eval (ModifyActiveFretboard fbInput next) = do
    s <- H.get
    let s2 = case fbInput of
               FB.ChordInput _ -> do
                 s { fretboards = fromMaybe s.fretboards (A.updateAt 0 ({input: fbInput, id: 0}) s.fretboards) }
               FB.NoChordInput -> s
    H.put s2
    pure next

  receiver :: Input -> Maybe (Query Unit)
  receiver (AddFretboardInput fbInput) = Just (AddFretboard fbInput unit)
  receiver (ModifyActiveFretboardInput fbInput) = Just (ModifyActiveFretboard fbInput unit)