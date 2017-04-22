module Cell where

import Control.Parallel

--GLUT
import Graphics.UI.GLUT
 
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
 
square :: GLfloat -> GLfloat -> GLfloat -> IO ()
square x y delt = renderPrimitive Quads $ mapM_ vertex3f
  [ ( x, y, 0), (x, (y - delt), 0), ((x + delt), (y - delt), 0), ((x + delt), y, 0) ]

nextGen :: [Int] -> Float -> [Int]
nextGen xs delt = par p1 (pseq p2 (p1 ++ p2)) --[y | y <- [0..(len^2 - 1)], isAlive xs y delt]
  where len = (round (2/delt))^2 - 1
        p1 = [y | y <- [0..(quot len 2)], isAlive xs y delt]
        p2 = [y | y <- [((quot len 2) + 1)..len], isAlive xs y delt]

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