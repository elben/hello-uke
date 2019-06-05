module Component.Search where

import Prelude

import Chords (Chord, humanChord, humanChordMod)
import Data.Array as A
import Data.Maybe (Maybe(..), isNothing)
import Data.String (null)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Notes (humanNote)
import Parser (findChords)
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent (key)

type State = { qs :: String          -- Query string
             , chord :: Maybe Chord  -- Selected chord
             , hover :: Maybe Chord  -- Chord user is hovering over
             , chords :: Array Chord -- Selectable chords
             , hide :: Boolean       -- Hide results
             , focus :: Boolean      -- Search is focused
             }

data Query a =
    -- Init a
    QueryStringChanged String a
  | ChordSelected Chord a
  | HideResults a
  -- TODO add keyboard support for [enter] and up/down
  -- TODO change selected chord as i type
  -- | KeyboardShortcut String a
  -- | HandleKey KeyboardEvent (H.SubscribeStatus -> a)

data Input =
    ClearQueryString -- Clear the query string
  | NoInput          -- No-op to differentiate between ClearQueryString (could also use Maybe)

-- Messages to send to parent component.
data Message =
    ChordSelectedMessage Chord
  | QueryStringChangedMessage

component :: forall m. H.Component HH.HTML Query Input Message m
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
          -- "HP.input_ InputSearch" was removed because Safari doesn't follow
          -- our CSS for some reason. Even DuckDuckGo doesn't use type="search", so.
          , HE.onValueInput (HE.input QueryStringChanged)
          , HE.onValueChange (HE.input QueryStringChanged)
          , HE.onFocusIn (HE.input_ (QueryStringChanged state.qs))
          -- , HE.onFocusOut (HE.input_ HideResults)

          -- https://github.com/slamdata/purescript-halogen/blob/master/examples/keyboard-input/src/Main.purs
          -- , HE.onKeyUp (HE.input (\e -> KeyboardShortcut (key e)))
          ]
        , HH.div
            [ HP.classes (append resultClasses [ ClassName "search-results" ]) ]
            (map
              (\c ->
                HH.div
                  [ HP.classes [ ClassName "search-result" ]
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
      let hover = if null q then Nothing else s.hover
      let s' = s {qs = q, chord = chord, hover = hover, chords = chords, hide = false}

      -- TODO if we do this type-ahead change, we need to be able to rever back
      -- to the last USER selected one (archived chord), and not a
      -- computer-selected one
      case A.head chords of
        Just c -> H.raise (ChordSelectedMessage c)
        _ -> pure unit
      -- Send this message too, which is used to reset the App state's chordSelectorChanged
      H.raise QueryStringChangedMessage
      H.put s'
      pure next
    ChordSelected chord next -> do
      H.modify_ (_ {qs = humanChord chord, chord = Just chord, hover = Just chord, chords = []})
      H.raise (ChordSelectedMessage chord)
      pure next
    HideResults next -> do
      H.modify_ (_ {hide = true})
      pure next
  --   KeyboardShortcut s -> do
  --     let focus = s == "s" || s == "/"
  --     H.modify_ (_ {focus = focus})
  --   HandleKey ev reply -> do
  --     evalKey (HandleKey ev reply)

  -- -- See https://github.com/slamdata/purescript-halogen/blob/v4.0.0/examples/keyboard-input/src/Main.purs
  -- evalKey (HandleKey ev reply)
  --   | KE.key ev == "Enter" = do
  --       -- s <- H.get
  --       -- H.modify_ (_ {qs = humanChord (trace "HandleKey enter" \_ -> chord), chord = Just chord, hover = Just chord, chords = []})
  --       pure (reply H.Done)
  --   | KE.key ev == "s" || KE.key ev == "/" = do
  --       pure (reply H.Done)

  initialState :: Input -> State
  initialState input = { qs: ""
                       , chord: Nothing
                       , hover: Nothing
                       , chords: []
                       , hide: false
                       , focus: true
                       }
  
  -- This component receives an Input from the parent component
  receiver :: Input -> Maybe (Query Unit)
  receiver NoInput = Nothing
  receiver ClearQueryString = Just (QueryStringChanged "" unit)