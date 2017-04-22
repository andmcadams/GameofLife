module Cell where
 
--GLUT
import Graphics.UI.GLUT
 
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
 
square :: GLfloat -> GLfloat -> GLfloat -> IO ()
square x y delt = renderPrimitive Quads $ mapM_ vertex3f
  [ ( x, y, 0), (x, (y - delt), 0), ((x + delt), (y - delt), 0), ((x + delt), y, 0) ]

nextGen :: [Int] -> Float -> [Int]
nextGen xs delt = [y | y <- [0..(len^2 - 1)], isAlive xs y delt]
  where len = round (2/delt)

isAlive :: [Int] -> Int -> Float -> Bool
isAlive xs cell delt = if elem cell xs then (alive - 1) == 3 || (alive - 1) == 2 else alive == 3
  where top = break (> (cell - len + 1)) $ snd (break (>= (cell - len - 1)) xs)
        mid = break (> (cell + 1)) $ snd (break (>= (cell - 1)) $ snd top)
        bot = break (> (cell + len + 1)) $ snd (break (>= (cell + len - 1)) $ snd mid)
        alive = (length $ [k | k <- (fst top), m == ((quot k len) + 1)]) + (length $ [k | k <- (fst mid), m == (quot k len)]) + (length $ [k | k <- (fst bot), m == ((quot k len) - 1)])
        len = round (2/delt)
        m = quot cell len