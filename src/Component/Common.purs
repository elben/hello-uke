module Component.Common where

import Chords
import Prelude

import Halogen.HTML as HH

chordHtml :: forall p i. Note -> ChordQuality -> ChordInterval -> Array (HH.HTML p i)
chordHtml (Note name pos) q i =
  [ HH.text name
  , divide
  , mod
  ]
  where
    divide = case q of
               Minor ->
                 case i of
                   Triad -> HH.text ""
                   Dom7 -> HH.text ""
                   _ -> HH.text "/"
               _ -> HH.text ""
    mod = case i of
            Triad -> HH.text ""
            Dom7 -> HH.sup [] [ HH.text "7" ]
            Maj7 -> HH.sup [] [ HH.text "M7" ]
            Second -> HH.text "2"
            Fourth -> HH.text "4"