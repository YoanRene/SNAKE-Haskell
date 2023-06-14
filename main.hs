import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.List
import System.IO.Unsafe
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

-- Define the game state
data GameState = GameState {
  snake :: [(Int, Int)],
  direction :: (Int, Int),
  food :: [((Int, Int),Int)],
  obstaculos :: [(Int,Int)],
  gameOver :: Bool,
  score :: Int
}


-- Define the initial game state
initialState :: GameState
initialState = GameState {
  obstaculos = randomItems  (0,0) cantObstaculos [], 
  snake = randomItems (0,0) 1 (obstaculos initialState) ,
  food = zip (randomItems (0,0) cantHuevos ((snake initialState)++(obstaculos initialState))) (cycle [1..cantHuevos]),
  direction = (1, 0),
  gameOver = False,
  score=0
}

-- Define la función roundedRectangle para dibujar un rectángulo con bordes suaves
roundedRectangle :: Float -> Float -> Float -> Picture
roundedRectangle width height radius = pictures [ 
  polygon [(radius, 0),
    (width - radius, 0),
    (width, radius),
    (width, height - radius),
    (width - radius, height),
    (radius, height),
    (0, height - radius),
    (0, radius)]]

removeElements :: Eq a => [a] -> [a] -> [a]
removeElements xs ys = filter (\x -> not (elem x ys)) xs

-- Define the window size and title
window :: Display
window = InWindow "Yoan-Kevin Snake" (640, 480) (10, 10)

cantHuevos :: Int
cantHuevos = 5

cantObstaculos :: Int
cantObstaculos = 7

verdeOscuro :: Color
verdeOscuro = makeColor 0.1 0.5 0.1 1.0

-- Define the size of each cell in the grid
cellSize :: Float
cellSize = 20.0

initNumerosHuevos :: Int -> [Int]-> [Int]
initNumerosHuevos 0 _ = []
initNumerosHuevos n xs =
  (xs !! x'):(initNumerosHuevos (n-1) (delete x' xs))
  where
    x1=randomRIO (0, n-1) :: IO Int
    x'= unsafePerformIO x1


randomItems :: (Int,Int) -> Int -> [(Int,Int)] -> [(Int,Int)]
randomItems _ 0 _ = []
randomItems (x, y) n invalids = 
  if (x', y') `elem` invalids 
    then randomItems (x', y') n invalids 
  else 
    (x', y'):(randomItems (x,y) (n-1) ((x',y'):invalids))
  where
    x1=randomRIO (-10, 10) :: IO Int
    y1=randomRIO (-10, 10) :: IO Int
    x'= unsafePerformIO x1
    y'= unsafePerformIO y1

updateFood :: [((Int,Int),Int)] -> (Int,Int) -> Int -> [(Int,Int)] -> [((Int,Int),Int)]
updateFood food (x,y) n invalids=
  if( length food == 1) then (zip (randomItems (0,0) cantHuevos invalids) (cycle [1..cantHuevos]))
  else filter (\((a,b),z) -> a/=x||b/=y) food

-- Define the update function
update :: Float -> GameState -> GameState
update _ gameState@(GameState { obstaculos = obstaculos ,snake = (x, y):xs, direction = (dx, dy), food = food, gameOver = False ,score=score}) =
    if x' < -10 || x' > 10 || y' < -10 || y' > 10 || (x', y') `elem` xs || (x', y') `elem` obstaculos
        then gameState { gameOver = True }
    else if (x', y') `elem` fst (unzip food)
        then gameState { snake = (x', y'):  (x,y):xs, food = updateFood food (x', y') cantHuevos (((x',y'):(x,y):xs)++obstaculos) ,score=score+1}
    else 
        gameState { snake = (x', y') : init ((x,y):xs) }
    where
        x' = (((x + 10 + dx) `mod` 21)-10)
        y' = (((y + 10 + dy) `mod` 21)-10)      
update _ gameState = gameState

-- Define the input handling function
isOpposite :: (Int, Int) -> (Int, Int) -> Bool
isOpposite (x1, y1) (x2, y2) = x1 == -x2 && y1 == -y2

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'w') _ _ _) gameState@(GameState { direction = dir })
  | not (isOpposite dir (0, 1)) = gameState { direction = (0, 1) }
handleInput (EventKey (Char 'a') _ _ _) gameState@(GameState { direction = dir })
  | not (isOpposite dir (-1, 0)) = gameState { direction = (-1, 0) }
handleInput (EventKey (Char 's') _ _ _) gameState@(GameState { direction = dir })
  | not (isOpposite dir (0, -1)) = gameState { direction = (0, -1) }
handleInput (EventKey (Char 'd') _ _ _) gameState@(GameState { direction = dir })
  | not (isOpposite dir (1, 0)) = gameState { direction = (1, 0) }
handleInput _ gameState = gameState

-- Define the rendering function
render :: GameState -> Picture
render GameState {obstaculos=obstaculos, snake = (x,y):xs, food = food, gameOver = False ,score=score} =
  pictures [renderCeldas,renderObstaculos obstaculos,renderSnake xs , renderFood (fst (unzip food)), renderFoodVerde (fst (unzip food)), renderNumeros food,renderHead (x,y) ,renderScore score]
  where
    renderHead (x,y) = color verdeOscuro (renderCell (x,y))
    mapa= [(a,b)| a <- [-10..11],b <- [-10..11]]
    renderSnake xs = color green (pictures (map renderCell xs))
    renderNumeros food = color white (pictures (map renderCellNumeroFood food))
    renderFood food = color red (pictures (map renderCell food))
    renderFoodVerde food = color verdeOscuro (pictures (map renderCellV food))
    renderObstaculos obstaculos= color blue (pictures (map renderCell obstaculos))
    renderCeldas = color black (pictures (map renderCellS mapa))
    renderScore score = translate (-310) 215 (scale 0.15 0.15 (color red (text ("Score: "++(show score)))))
    renderCellNumeroFood ((x,y),z) = (translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (scale 0.18 0.18 (text (show z))))
    renderCellS (x, y) = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (rectangleSolid cellSize cellSize)
    renderCellV (x, y) = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (roundedRectangle (cellSize/3) (cellSize/3) 2)
    renderCell (x, y) = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (roundedRectangle cellSize cellSize 4)
render GameState { gameOver = True ,score=score} = pictures
  [translate (-200) 0 (scale 0.5 0.5 (color red (text ("Game Over")))),
  translate (-135) (-90) (scale 0.4 0.4 (color red (text ("Score: "++(show score)))))]
  
-- Define the main function
main :: IO ()
main = play window white 6 initialState render handleInput update