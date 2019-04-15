#!/usr/bin/env cabal

{- cabal:

  build-depends: base       ^>= 4.12
               , bytestring ^>= 0.10
               , containers ^>= 0.6

               , binary     ^>= 0.8
               , zlib       ^>= 0.6

-}

--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE BlockArguments    #-}

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "zlib" Codec.Compression.GZip as GZip

--------------------------------------------------

import qualified "binary" Data.Binary as Binary

--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

--import qualified "bytestring" Data.ByteString.Char8 as ASCII

import qualified "bytestring" Data.ByteString.Lazy  as ByteString
import           "bytestring" Data.ByteString.Lazy (ByteString)

--------------------------------------------------

import qualified "base" Control.Monad as Monad

--------------------------------------------------
-- Main ------------------------------------------
--------------------------------------------------

main = do

  let value = Monad.join (Map.lookup "ghc" tableMap)

  print value

--ByteString.writeFile "./table.gz" tableBytes

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

--
-- this is a gzip-compressed literal bytestring, storing a binary-encoded Data.Map.
--

tableBytes :: ByteString
tableBytes =

    "\US\139\b\NUL\NUL\NUL\NUL\NUL\NUL\ETXEN\
    \\219\SO\194 \f\197\224\188\196\CAN\227\US\
    \\224\171~\NAKc\GS4ce\161`\178\191\215(\176\
    \\190\180\167\231\210\n\241\171\203\191\ti\
    \\157\217\149\249< \ENQ\214\&9>\202\162\179a\
    \\132X\233\ESC=\231\215\164\SYN\157\DC2D\226*\
    \\146\174o\t\167\DLE\209\"i_\240\193\129\199<W\
    \\250nC\CAN\212\CAN\162J\160\141C\178\133\216;\
    \\\@4\144-W\203\209x\205\140\166\RS\163\237]9f\
    \\170\143\ACK\163g\223\STX\184\&7\rH\222\FSW\
    \\130\&7D\197\NUL\164\&0U\193\186\t\186o\
    \\228\180~\NUL\a6\249\137#\SOH\NUL\NUL"

--------------------------------------------------

-- build the table from the bytestring:

tableMap :: Map String (Maybe String)
tableMap = go tableBytes
  where

  go
    = Binary.decode
    . GZip.decompress

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------