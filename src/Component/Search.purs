module Component.Search where

import Prelude

import Chords (Chord, humanChord)
import Data.Array as A
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.String (null)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Parser (findChords)
import Web.Event.Event as Event
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent as KE

type State = { qs :: String          -- Query string
             , chord :: Maybe Chord  -- Selected chord
             , chordIdx :: Int       -- For keyboard up/down, the current index being selected
             , chords :: Array Chord -- Selectable chords
             , hide :: Boolean       -- Hide results
             }

data Query a =
    -- Init a
    QueryStringChanged String a
  | ChordSelected Chord a
  | HideResults a
  | KeyboardAction KE.KeyboardEvent a
  -- | HandleKey KeyboardEvent (H.SubscribeStatus -> a)

data Input =
    ClearQueryString -- Clear the query string
  | NoInput          -- No-op to differentiate between ClearQueryString (could also use Maybe)

-- Messages to send to parent component.
data Message =
    ChordSelectedMessage Chord       -- User manually chooses a chord
  | ChordLookaheadMessage Chord      -- Auto-selection of chord as user types
  | QueryStringChangedMessage String

-- MonadEffect m evidence needed to use liftEffect.
component :: forall m. MonadEffect m => H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState
    , render
    , eval
    -- , initializer: Just (H.action Init)
    , receiver
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    let resultClasses = if state.hide then [ ClassName "hide" ] else []
    in
      HH.div
        [ HP.classes [ ClassName "search" ] ]
        [ HH.input
          [ HP.placeholder "Search for chords (e.g. Cm7)"
          , HP.autocomplete false
          , HP.spellcheck false
          , HP.value state.qs
          , HE.onValueInput (HE.input QueryStringChanged)
          , HE.onFocusIn (HE.input_ (QueryStringChanged state.qs))
          -- , HE.onFocusOut (HE.input_ HideResults)

          -- https://github.com/slamdata/purescript-halogen/blob/master/examples/keyboard-input/src/Main.purs
          , HE.onKeyDown (HE.input KeyboardAction)
          ]
        , HH.div
            [ HP.classes (append resultClasses [ ClassName "search-results" ]) ]
            (mapWithIndex
              (\idx c ->
                let classes = if idx == state.chordIdx then [ ClassName "selected" ] else []
                in HH.div
                     [ HP.classes (append classes [ ClassName "search-result" ])
                     , HE.onClick (HE.input_ (ChordSelected c))
                     ]
                     [ HH.text (humanChord c) ]
              )
              state.chords)
        ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval query = case query of
    -- Init next -> do
    --   document <- H.liftEffect $ DOM.document =<< DOM.window
    --   H.subscribe $ ES.eventSource' (K.onKeyUp document) (Just <<< H.request <<< HandleKey)
    --   pure next
    QueryStringChanged q next -> do
      let chords = findChords 5 q
      -- If something was typed, unmark the previously selected chord.
      s <- H.get
      let chord = if null q then Nothing else s.chord
      let s' = s {qs = q, chord = chord, chords = chords, hide = false}

      -- Take the first result and "select" this chord. This will make the
      -- active fretboard change as you type.
      case A.head chords of
        Just c -> H.raise (ChordLookaheadMessage c)
        _ -> pure unit

      -- Send this message too, which is used to reset the App state's chordSelectorChanged
      H.raise (QueryStringChangedMessage q)
      H.put s'
      pure next
    ChordSelected chord next -> do
      H.modify_ (_ {qs = humanChord chord, chord = Just chord, chords = []})
      H.raise (ChordSelectedMessage chord)
      pure next
    HideResults next -> do
      H.modify_ (_ {hide = true})
      pure next
    KeyboardAction ke next -> do
      let key = KE.key ke

      s <- H.get

      let arrow = key == "ArrowDown" || key == "ArrowUp"
      let enter = key == "Enter"

      let idx = if key == "ArrowDown"
                  then min (s.chordIdx + 1) ((A.length s.chords) - 1)
                  else if key == "ArrowUp"
                    then max (s.chordIdx - 1) 0
                    else s.chordIdx

      -- Maybe Chord
      let chord = A.index s.chords idx

      -- On [Enter], erase results since we've selected a chord.
      let chords = if enter then [] else s.chords

      -- On an action, set the query string to the chord's human string.
      let qs = if arrow || enter then maybe s.qs humanChord chord else s.qs

      -- On [Enter], send an actual chord selected msg. Otherwise, just send a
      -- lookahead chord msg.
      let msgType = if enter then ChordSelectedMessage else ChordLookaheadMessage

      -- Send the msg, if we can find a chord.
      maybe (pure unit) (H.raise <<< msgType) chord

      -- Prevent up/down arrow keys from actually moving the cursor in the
      -- <input>. Lift the `Effect Unit` to type of this function. This line
      -- where is why we need evidence of `MonadEffect m` in this component, and
      -- in App.
      -- https://stackoverflow.com/questions/1080532/prevent-default-behavior-in-text-input-while-pressing-arrow-up
      -- https://github.com/slamdata/purescript-halogen/issues/426
      when (arrow) $ H.liftEffect (Event.preventDefault (KE.toEvent ke))

      -- Reset index if [Enter], but do it here, once all processing is done
      -- using the current idx.
      let s' = s { qs = qs
                 , chordIdx = if enter then 0 else idx
                 , chord = chord
                 , chords = chords
                 }
      H.put s'

      pure next
  --   HandleKey ev reply -> do
  --     evalKey (HandleKey ev reply)

  -- -- See https://github.com/slamdata/purescript-halogen/blob/v4.0.0/examples/keyboard-input/src/Main.purs
  -- evalKey (HandleKey ev reply)
  --   | KE.key ev == "Enter" = do
  --       -- s <- H.get
  --       -- H.modify_ (_ {qs = humanChord (trace "HandleKey enter" \_ -> chord), chord = Just chord, chords = []})
  --       pure (reply H.Done)
  --   | KE.key ev == "s" || KE.key ev == "/" = do
  --       pure (reply H.Done)

  initialState :: Input -> State
  initialState input = { qs: ""
                       , chord: Nothing
                       , chordIdx: 0
                       , chords: []
                       , hide: false
                       }
  
  -- This component receives an Input from the parent component
  receiver :: Input -> Maybe (Query Unit)
  receiver NoInput = Nothing
  receiver ClearQueryString = Just (QueryStringChanged "" unit)