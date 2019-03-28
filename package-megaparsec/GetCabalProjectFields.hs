#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "ghc.withPackages (ps: with ps; [ turtle text megaparsec parser-combinators ])"

--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------

{-# LANGUAGE PackageImports #-}

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--import "turtle" Turtle

import qualified "turtle" Turtle.Prelude as H     -- Mnemonic: « H » for "sHell". 
import qualified "turtle" Turtle.Bytes   as Bytes
import qualified "turtle" Turtle.Format  as Format
import qualified "turtle" Turtle.Line    as Line
import qualified "turtle" Turtle.Options as Options
import qualified "turtle" Turtle.Pattern as Pattern
import qualified "turtle" Turtle.Shell   as Shell

--------------------------------------------------

import qualified "parser-combinators" Control.Monad.Combinators.Expr as Expr

import qualified "megaparsec" Text.Megaparsec            as P
import qualified "megaparsec" Text.Megaparsec.Char       as Char
import qualified "megaparsec" Text.Megaparsec.Char.Lexer as Lexer

--------------------------------------------------

import qualified "text" Data.Text as T

import           "text" Data.Text (Text)

--------------------------------------------------

import "base" Data.Void

--------------------------------------------------

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

type Parser = P.Parsec Void Text

--------------------------------------------------

{- 

e.g. this @reStructuredText@ fragment:

@
.. cfg-field:: <NAME>: <TYPE>
               ...
               ...
    :synopsis: <SYNOPSIS>

    :default: ``<DEFAULT>``

    <DESCRIPTION>
@

is parsed into this @Haskell@ record:

@
= ParsedField
  { name         = <NAME>
  , kind         = <TYPE>
  , defaultValue = <DEFAULT>
  , synopsis     = <SYNOPSIS>
  , description  = <DESCRIPTION>
  }
@

-}

data ParsedField = ParsedField
  { name         :: String
  , kind         :: String
  , defaultValue :: String
  , synopsis     :: String
  , description  :: String
  }

--------------------------------------------------
-- Values ----------------------------------------
--------------------------------------------------

default_RstFilePath = "./cabal/Cabal/doc/nix-local-build.rst"

--------------------------------------------------



--------------------------------------------------

sc :: Parser ()
sc = Lexer.space Char.space1 lineCmnt blockCmnt
  where
  lineCmnt  = Lexer.skipLineComment "//"
  blockCmnt = Lexer.skipBlockComment "/*" "*/"

-- sc stands for “space consumer”. space takes three arguments: a parser that parses whitespace (but it should not accept empty input), a parser for line comments, and a parser for block (multi-line) comments.

--------------------------------------------------

-- lexeme :: Parser a -> Parser a
-- lexeme = Lexer.lexeme sc

-- --------------------------------------------------

-- symbol :: String -> Parser String
-- symbol = Lexer.symbol sc

--------------------------------------------------

main = do

    H.cd     "/tmp"
    H.mkdir  "test"
    H.output "test/foo" "Hello, world!"
    H.stdout (H.input "test/foo")
    H.rm     "test/foo"
    H.rmdir  "test"
    H.sleep  1
    H.die    "Urk!"

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

-- main = do
--
--     cd     "/tmp"
--     mkdir  "test"
--     output "test/foo" "Hello, world!"  -- Write "Hello, world!" to "test/foo"
--     stdout (input "test/foo")          -- Stream "test/foo" to stdout
--     rm     "test/foo"
--     rmdir  "test"
--     sleep  1
--     die    "Urk!"

--------------------------------------------------

-- e.g. the « tests » field:
--
-- .. cfg-field:: tests: boolean
--                --enable-tests
--                --disable-tests
--     :synopsis: Build tests.
--
--     :default: ``False``
--
--     Force test suites to be enabled.
--     ...
--
--     The command line variant of this flag is ``--enable-tests`` and
--     ``--disable-tests``.
--
-- i.e.:
--
-- .. cfg-field:: <NAME>: <TYPE>
--                ...
--                ...
--     :synopsis: <SYNOPSIS>
--
--     :default: ``<DEFAULT>``
--
--     <DESCRIPTION>

--------------------------------------------------

-- CABAL_PROJECT_RST=${1:-"./cabal/Cabal/doc/nix-local-build.rst"}

-- runhaskell GetCabalProjectFields.hs "$(< "${CABAL_PROJECT_RST}")"

-- # e.g. a field:
-- #
-- # .. cfg-field:: tests: boolean
-- #                --enable-tests
-- #                --disable-tests
-- #     :synopsis: Build tests.
-- #
-- #     :default: ``False``
-- #
-- #     Force test suites to be enabled.
-- #
-- #     ...

-- # parse-cabal-project-reStructuredText.sh

--------------------------------------------------

--------------------------------------------------