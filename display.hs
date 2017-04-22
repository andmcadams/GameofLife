module Display (display, idle, timeout) where

import Control.Monad
import Data.IORef
--GLUT
import Graphics.UI.GLUT

import Cell
 
reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

points :: [Int] -> Float -> [(GLfloat,GLfloat,GLfloat)]
points ks delt = [ ( (-1 + delt*(fromIntegral (mod k len))), (1 - delt*(fromIntegral (quot k len))), 0) | k <- ks ]
  where len = round (2/delt)

display :: IORef [Int] -> Float -> DisplayCallback
display cells delt = do
  clearColor $= Color4 0 0 0 (1 :: GLfloat)
  clear [ColorBuffer]
  loadIdentity
  preservingMatrix $ do
    c <- get cells
    forM_ (points c delt) $ \(x,y,z) ->
      preservingMatrix $ do
        color $ Color3 1 1 (1 :: GLfloat)
        square x y delt
  swapBuffers

timeout :: IORef [Int] -> Float -> Int -> IO ()
timeout cells delt delay = do
  cells $~! \c -> (nextGen c delt)
  addTimerCallback delay (timeout cells delt delay)

idle :: IdleCallback
idle = do
  postRedisplay Nothing