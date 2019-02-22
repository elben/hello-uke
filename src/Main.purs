module Main where

import Prelude

import ChordSelector as ChordSelector
import Effect (Effect)
import Effect (Effect)
import Effect.Console (log)
import Fretboard as Fretboard
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ChordSelector.chordSelectorComponent unit body
