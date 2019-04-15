#!/usr/bin/env cabal

{- cabal:

  build-depends: base       ^>= 4.12
               , text       ^>= 1.2
               , formatting ^>= 6.3

-}

--------------------------------------------------
--- Extensions -----------------------------------
--------------------------------------------------

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}

--------------------------------------------------

{- | Example for the @formatting@ package.

See:

* <http://hackage.haskell.org/package/formatting/docs/Formatting.html Formatting>

-}

module Main where

--------------------------------------------------
--- Imports --------------------------------------
--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting (Format, (%))

--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------

import qualified "base" System.IO    as IO

--------------------------------------------------
--- Main -----------------------------------------
--------------------------------------------------

main :: IO ()
main = do

  putStrLn example

--------------------------------------------------

example :: String
example = Format.formatToString

      ( "\nString:     " % Format.string
      % "\nText:       " % Format.stext
      % "\nShow:       " % Format.shown

      % "\nCardinal:   " % Format.int
      % "\nOrdinal:    " % Format.ords

      % "\nQuantity:   " % Format.commas
      % "\nPluralized: " % Format.plural "person" "people"

      % "\n")

      "abc"             -- “abc”
      "abc"             -- “abc”
      ("abc" :: String)  -- “"abc"”

      1                 -- “1”
      2                 -- “2nd”

      (33000 :: Int)     -- “33,000”
      4                 -- (4) “people”

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------