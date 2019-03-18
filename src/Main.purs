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

awaitRoot :: Aff HTMLElement
awaitRoot = do
  awaitLoad
  body <- selectElement (QuerySelector "div.structure")
  maybe (throwError (error "Could not find body")) pure body
  