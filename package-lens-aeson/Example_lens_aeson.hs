{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------

{-| 


-}

module Example_lens_aeson where

--------------------------------------------------

import qualified "lens-aeson" Data.Aeson.Lens as JSON

--------------------------------------------------

main_lens_aeson = do

  putStrLn "[o]"
  print $ object
  print $ object JSON.^? JSON._Value

  putStrLn "[o.a]"       
  print $ objectDotA
  print $ Just (JSON.String "xyz")

  putStrLn "[o.b]"       
  print $ objectDotA
  print $ Nothing

  
--------------------------------------------------

object = "{\"a\": \"xyz\", \"b\": null}"

--------------------------------------------------

objectDotA = object JSON.^? (JSON.key "a" . JSON.nonNull)

--------------------------------------------------

objectDotB = object JSON.^? (JSON.key "b" . JSON.nonNull)

--------------------------------------------------