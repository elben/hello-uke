module Parser where

import Notes
import Prelude hiding (between,when)

import Chords (Chord, ChordInterval(..), ChordQuality(..), chordIntervals, chordQualities, ukeChordExists)
import Control.Alt ((<|>))
import Data.Array (snoc, take)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.String (Pattern(..), Replacement(..), replaceAll, toLower, toUpper)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (option, optionMaybe, try)
import Text.Parsing.Parser.String (string)

findChords:: Int -> String -> Array Chord
findChords n s = case parseString n s of
              Right chrds -> chrds
              Left _ -> []

parseString :: Int -> String -> Either ParseError (Array Chord)
parseString n s = runParser (replaceAll (Pattern " ") (Replacement "") s) (chords n)

chords :: Int -> Parser String (Array Chord)
chords max = do
  ns <- note
  qs <- option chordQualities quality
  _  <- optionMaybe (string "/") -- optional / separator. Ex: Cm/M7.
  is <- option chordIntervals interval

  -- Tripe for-loop across the notes, qualities and intervals. Assumes that
  -- order matters (most-likely first) in the ns, qs and is arrays.
  -- TODO lazily find until max
  let chrds = foldl
                (\acc n@(Note _ _ p) ->
                  foldl
                  (\acc' q ->
                    foldl (\acc'' i ->
                            if ukeChordExists p q i
                            then snoc acc'' {note: n, quality: q, interval: i}
                            else acc'')
                    acc' is)
                  acc qs)
                [] ns
-- TODO make this lazy
--   pure (take max chrds)
  pure (take max chrds)

note :: Parser String (Array Note)
note = 
  -- The multi-character strings need to come before the singles
      try (anyCase "c" >>= sharp >>= opts [cs])
  <|> try (anyCase "c"           >>= opts [c, cs])
  <|> try (anyCase "d" >>= sharp >>= opts [ds])
  <|> try (anyCase "d" >>= flat  >>= opts [df])
  <|> try (anyCase "d"           >>= opts [d, ds, df])
  <|> try (anyCase "e" >>= flat  >>= opts [ef])
  <|> try (anyCase "e"           >>= opts [e, ef])
  <|> try (anyCase "f" >>= sharp >>= opts [fs])
  <|> try (anyCase "f"           >>= opts [f, fs])
  <|> try (anyCase "g" >>= sharp >>= opts [gs])
  <|> try (anyCase "g" >>= flat  >>= opts [gf])
  <|> try (anyCase "g"           >>= opts [g, gs, gf])
  <|> try (anyCase "a" >>= sharp >>= opts [as])
  <|> try (anyCase "a" >>= flat  >>= opts [af])
  <|> try (anyCase "a"           >>= opts [a, as, af])
  <|> try (anyCase "b" >>= flat  >>= opts [bf])
  <|> try (anyCase "b"           >>= opts [b, bf])

anyCase :: String -> Parser String String
anyCase n = string (toLower n) <|> string (toUpper n)

quality :: Parser String (Array ChordQuality)
quality =
      (try (string "major" <|> string "maj") >>= \_ -> pure [Major])
  <|> (try (string "minor" <|> string "min") >>= \_ -> pure [Minor])
  <|> (try (string "M") >>= \_ -> pure [Major, Minor])
  <|> (try (string "m") >>= \_ -> pure [Minor, Major])
  <|> (try (string "sus") >>= \_ -> pure [Suspended])

interval :: Parser String (Array ChordInterval)
interval =
      (try (string "dom7") >>= opts [Dom7])
  <|> (try (string "maj7" <|> anyCase "M7") >>= opts [Maj7])
  <|> (try (string "7") >>= \_ -> pure [Dom7, Maj7])
  <|> (try (string "dom9" <|> string "9") >>= opts [Dom9])
  <|> (try (string "2") >>= opts [Second])
  <|> (try (string "4") >>= opts [Fourth])

-- Helper to avoid awkward syntax
sharp :: String -> Parser String Accidental
sharp _ = (string "#" <|> string "♯") >>= \_ -> pure Sharp

-- Helper to avoid awkward syntax
flat :: String -> Parser String Accidental
flat _ = (string "b" <|> string "♭") >>= \_ -> pure Flat

-- Helper to avoid awkward syntax of:
-- try (name "d" >>= \_ -> pure [d, ds, df])
opts :: forall a b. b -> a -> Parser String b
opts res _ = pure res