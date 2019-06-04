module Main where

import Prelude

import Component.App as App
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Halogen.Aff (awaitLoad, selectElement)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (HTMLElement)

main :: Effect Unit
main = HA.runHalogenAff do
  root <- awaitRoot
  runUI App.component unit root

-- Copy-pasted from Halogen source. We want to inject the main app into a specific HTML element.
awaitRoot :: Aff HTMLElement
awaitRoot = do
  awaitLoad
  body <- selectElement (QuerySelector "div.app")
  maybe (throwError (error "Could not find body")) pure body
