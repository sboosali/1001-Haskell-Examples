{-# LANGUAGE OverloadedStrings #-}

module Example.FLTKHS where

import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL as FL

-- This code creates a `TextEditor` then everything works as normal even with a
-- `handleCustom` function. In particular selecting text works.
--
-- 1. If you adjust the commented lines below so that this code creates a
--    `TextDisplay` then selecting text stops working.
--
-- 2. If you next remove the `handleCustom` function by commenting the record
--    update brackets then selecting text is restored.
--
-- How can you preserve the ability to select text in a TextDisplay while also
-- providing a handleCustom function?

ui :: IO ()
ui = do
    window <- windowNew
        (Size (Width 640) (Height 480))
        Nothing -- optional position
        (Just "a window")

    textBuff <- textBufferNew Nothing Nothing
    setText textBuff $ "fi fi fo fum\ni smell a human"

--  w <- textDisplayCustom
    w <- textEditorCustom
        (Rectangle {rectanglePosition = Position (X 24) (Y 24),
                    rectangleSize = Size (Width 320) (Height 240)})
        (Just "some text")
        Nothing -- optional draw-function
        (Just defaultCustomWidgetFuncs
{- delete the following line 
-}
            { handleCustom = Just $ \ref event -> do
                print $ show ref ++ " handler: " ++ show event
                handleSuper ref event
            }
--}
        )
    setBuffer w $ Just textBuff

    add window w
    showWidget window

main :: IO ()
main = ui >> FL.run >> FL.flush

