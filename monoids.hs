#!/usr/bin/env cabal

{- cabal:

  build-depends: base                 ^>= 4.12
               , optparse-applicative ^>= 0.14
               , deepseq              ^>= 1.4

-}

--------------------------------------------------
--- Extensions -----------------------------------
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Main where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "deepseq" Control.DeepSeq as Deep

--------------------------------------------------

import qualified "base" Control.Exception   as E
import qualified "base" System.Environment  as IO

import qualified "base" Data.Monoid    as Monoid
import qualified "base" Data.Semigroup as Semigroup

import           "base" Data.Function ((&))
import           "base" Data.Foldable

--------------------------------------------------

import "base" Prelude

--------------------------------------------------
-- Main ------------------------------------------
--------------------------------------------------

{-| 

-}

main :: IO ()
main = do

  IO.getArgs >>= mainWith

--------------------------------------------------

{-| 

-}

mainWith :: [String] -> IO ()
mainWith arguments = do

  putStrLn "\n----------------------------------------\n"

  putStrLn `traverse_` arguments

  putStrLn "\n----------------------------------------\n"

  print =<< firstEnvironmentVariable "empty" []
  -- ⇒ "empty"

  putStrLn "\n----------------------------------------\n"

  print =<< firstEnvironmentVariable "default" [ "UNDEFINED_VARIABLE", "HOME", "XDG_RUNTIME_HOME" ]
  -- ⇒ "/home/sboo"

  putStrLn "\n----------------------------------------\n"

  return ()

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-| Return the value of the first environment variable that's been set, or a default value if all are unset.

Examples:

@
> firstEnvironmentVariable \"\/usr\/run\" [ \"XDG_RUNTIME_HOME\", \"TMP\" ]
@

Properties:

@
firstEnvironmentVariable x [] ≡ return x
@

-}

firstEnvironmentVariable :: String -> [String] -> IO String
firstEnvironmentVariable x0 ns = do

  x' <- foldrM go Nothing ns

  let x = x' & maybe x0 Semigroup.getFirst

  E.evaluate (Deep.force x)

  return x

  where

  go :: String -> Maybe (Semigroup.First String) -> IO (Maybe (Semigroup.First String))
  go n y = do

    x <- IO.lookupEnv n

    let x' = Semigroup.First <$> x
    -- TODO not lazy enough: (if n == "XDG_RUNTIME_HOME" then error "xxx" else x)

    let z = x' <> y

    return z

--------------------------------------------------

{-
firstNonemptyEnvironmentVariable :: String -> [String] -> IO String
firstNonemptyEnvironmentVariable x0 ns = do

  x <- getFirst <$> foldrM go (First x0) ns

  E.evaluate (Deep.force x)

  return x

  where

  go :: String -> String -> m String
  go n x = do

    y <- lookupEnv n

    let z = x <> First y

    return z
-}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b

--------------------------------------------------

>>> import qualified Data.Monoid    as Monoid
>>> Monoid.getFirst (Monoid.First (Just "hello") <> Monoid.First Nothing <> Monoid.First (Just "world"))
Just "hello"

>>> import qualified Data.Semigroup as Semigroup
>>> Semigroup.getFirst <$> (Semigroup.First "hello" <> Semigroup.First "world")
"hello"

>>> import qualified Data.Semigroup as Semigroup
>>> Semigroup.getFirst (Semigroup.First (Just "hello") <> Semigroup.First Nothing <> Semigroup.First (Just "world"))
Just "hello"

>>> import qualified Data.Semigroup as Semigroup
>>> Semigroup.getFirst ((Semigroup.First <$> (Just "hello")) <> (Semigroup.First <$> Nothing) <> (Semigroup.First <$> (Just "world")))
Just "hello"

>>> import qualified Data.Semigroup as Semigroup
>>> Semigroup.getFirst <$> (Just (Semigroup.First "hello") <> Nothing <> Just (Semigroup.First "world"))
Just "hello"

--------------------------------------------------

--------------------------------------------------

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------