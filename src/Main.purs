module Main where

import Prelude

import Component.ChordSelector as ChordSelector
import Component.Fretboard as Fretboard
import Component.App as App

import Effect (Effect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI App.component unit body
