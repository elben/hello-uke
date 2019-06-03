module Parser where

import Notes
import Prelude hiding (between,when)

import Chords (Chord, ChordInterval(..), ChordQuality(..))
import Control.Alt ((<|>))
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.String (Pattern(..), Replacement(..), replaceAll, toLower)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (try, option)
import Text.Parsing.Parser.String (string)

compute :: String -> Array Chord
compute s = case parseString s of
              Right chrds -> chrds
              Left _ -> []

-- TODO we don't want to lower-case "M" since that tells us major or minor.
-- Except, the words "major" or "minor" or "maj" or "min" we can.
parseString :: String -> Either ParseError (Array Chord)
parseString s = runParser (toLower (replaceAll (Pattern " ") (Replacement "") s)) chords

chords :: Parser String (Array Chord)
chords = do
  ns <- note
  qs <- option [Major] quality
  is <- option [Triad] interval

  -- Tripe for-loop across the notes, qualities and intervals. Assumes that
  -- order matters (most-likely first) in the ns, qs and is arrays.
  let chrds = foldl
                (\acc n ->
                  foldl
                  (\acc' q ->
                    foldl (\acc'' i -> snoc acc'' {note: n, quality: q, interval: i})
                    acc' is)
                  acc qs)
                [] ns
  pure chrds

note :: Parser String (Array Note)
note = 
  -- The multi-character strings need to come before the singles
      try (string "c" >>= \_ -> sharp >>= \_ -> pure [cs])
  <|> try (string "c" >>= \_ -> pure [c, cs])
  <|> try (string "d" >>= \_ -> sharp >>= \_ -> pure [ds])
  <|> try (string "d" >>= \_ -> flat >>= \_ -> pure [df])
  <|> try (string "d" >>= \_ -> pure [d, ds, df])
  <|> try (string "e" >>= \_ -> flat >>= \_ -> pure [ef])
  <|> try (string "e" >>= \_ -> pure [e, ef])
  <|> try (string "f" >>= \_ -> sharp >>= \_ -> pure [fs])
  <|> try (string "f" >>= \_ -> pure [f, fs])
  <|> try (string "g" >>= \_ -> sharp >>= \_ -> pure [gs])
  <|> try (string "g" >>= \_ -> flat >>= \_ -> pure [gf])
  <|> try (string "g" >>= \_ -> pure [g, gs, gf])
  <|> try (string "a" >>= \_ -> sharp >>= \_ -> pure [as])
  <|> try (string "a" >>= \_ -> flat >>= \_ -> pure [af])
  <|> try (string "a" >>= \_ -> pure [a, as, af])
  <|> try (string "b" >>= \_ -> flat >>= \_ -> pure [bf])
  <|> try (string "b" >>= \_ -> pure [b, bf])

quality :: Parser String (Array ChordQuality)
quality =
      (try (string "major" <|> string "maj") >>= \_ -> pure [Major])
  <|> (try (string "minor" <|> string "min") >>= \_ -> pure [Minor])
  <|> (try (string "M") >>= \_ -> pure [Major, Minor])
  <|> (try (string "m") >>= \_ -> pure [Minor, Major])
  <|> (try (string "sus") >>= \_ -> pure [Suspended])

interval :: Parser String (Array ChordInterval)
interval =
      (try (string "dom7") >>= \_ -> pure [Dom7])
  <|> (try (string "maj7") >>= \_ -> pure [Maj7])
  <|> (try (string "7") >>= \_ -> pure [Dom7, Maj7])
  <|> (try (string "dom9" <|> string "9") >>= \_ -> pure [Dom9])
  <|> (try (string "2") >>= \_ -> pure [Second])
  <|> (try (string "4") >>= \_ -> pure [Fourth])

sharp :: Parser String Accidental
sharp = (string "#" <|> string "♯") >>= \_ -> pure Sharp

flat :: Parser String Accidental
flat = (string "b" <|> string "♭") >>= \_ -> pure Flat