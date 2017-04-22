module Bindings (reshape, keyboardMouse) where

import Data.IORef 
import System.Exit

--GLUT
import Graphics.UI.GLUT

import Cell

reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

keyboardMouse :: IORef [Int] -> Float -> KeyboardMouseCallback
keyboardMouse a delt key Down _ _ = case key of
  (SpecialKey KeyRight) -> a $~! \c -> (nextGen c delt)
  (Char 'x') -> die "Shutting down Game of Life."
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()
