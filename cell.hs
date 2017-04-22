module Cell where

import Control.Parallel
import Data.List

--GLUT
import Graphics.UI.GLUT
 
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
 
square :: GLfloat -> GLfloat -> GLfloat -> IO ()
square x y delt = renderPrimitive Quads $ mapM_ vertex3f
  [ ( x, y, 0), (x, (y - delt), 0), ((x + delt), (y - delt), 0), ((x + delt), y, 0) ]

nextGen :: [Int] -> Float -> [Int]
nextGen xs delt = par p1 (pseq p2 (p1 ++ p2)) --[y | y <- [0..(len^2 - 1)], isAlive xs y delt]
  where len = (rowlen)^2 - 1
        rowlen = round (2/delt)
        p = nub $ concat [[(d - 1), (d), (d + 1),
              (x - 1), (x), (x + 1),
              (s - 1), (s), (s + 1)] | x <- xs, let d = x - rowlen, let s = x + rowlen]
        sp = splitAt (quot (length p) 2) p
        b = [(min (xs !! 0 - rowlen - 1) 0)..(max (xs !! (length xs - 1) + rowlen + 1) len)]
        sb = splitAt (quot (length b) 2) b
        p1 = if length xs < (round (0.2* fromIntegral len)) -- might want to check if they are dense on an interval instead, better results
             then [y | y <- fst sp, isAlive xs y delt] 
             else [y | y <- fst sb, isAlive xs y delt]
        p2 = if length xs < (round (0.2* fromIntegral len))
             then [y | y <- snd sp, isAlive xs y delt]
             else [y | y <- snd sb, isAlive xs y delt]

isAlive :: [Int] -> Int -> Float -> Bool
isAlive xs cell delt
                    | (toplen + midlen) > 3 = False
                    | alive == 3 = True
                    | alive == 2 = elem cell xs
                    | otherwise = False
                      where top = break (> (cell - len + 1)) $ snd (break (>= (cell - len - 1)) xs)
                            mid = break (> (cell + 1)) $ snd (break (>= (cell - 1)) $ snd top)
                            bot = break (> (cell + len + 1)) $ snd (break (>= (cell + len - 1)) $ snd mid)
                            toplen = length [k | k <- (fst top), m == ((quot k len) + 1)]
                            midlen = length [k | k <- (fst mid), m == (quot k len), k /= cell]
                            botlen = length [k | k <- (fst bot), m == ((quot k len) - 1)]
                            alive = toplen + midlen + botlen
                            len = round (2/delt)
                            m = quot cell len