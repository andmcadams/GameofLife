import Data.Char
import Data.IORef
import System.IO

--GLUT
import Graphics.UI.GLUT
--strings-1.1
import Data.Strings

import Bindings
import Display

main :: IO ()
main = do
  settings <- readFile "settings.txt"
  map <- readFile "map.txt"
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Game of Life"
  windowSize $= Size 400 400
  arr <- newIORef (createMap map)
  let delt = case lookup "size" (initSettings settings) of
           Just s -> 2 / (read s::Float)
           _ -> 0.1
      delay = case lookup "timer" (initSettings settings) of
            Just s -> round ((read s::Float)*1000)
            _ -> 1000
  displayCallback $= display arr delt
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse arr delt)
  addTimerCallback delay (timeout arr delt delay)
  idleCallback $= Just idle
  mainLoop

initSettings :: String -> [(String, String)]
initSettings str = initSettings' sp
  where sp = strSplitAll "\n" str

initSettings' :: [String] -> [(String, String)]
initSettings' (str:strs) = (strSplit "=" str) : (initSettings' strs)
initSettings' [] = []

createMap :: String -> [Int]
createMap str = createMap' str 0

createMap' :: String -> Int -> [Int]
createMap' (x:xs) count
                     | not (isDigit x) = createMap' xs count
                     | (digitToInt x) == 1 = count : (createMap' xs (count + 1))
                     | otherwise = createMap' xs (count + 1)
createMap' [] count = []