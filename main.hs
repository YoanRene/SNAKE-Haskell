import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.List
import System.IO.Unsafe

-- Define the game state
data GameState = GameState {
  snake :: [(Int, Int)],
  direction :: (Int, Int),
  food :: [(Int, Int)],
  vacio :: [(Int,Int)],
  gameOver :: Bool,
  score :: Int
}

-- Define the initial game state
initialState :: GameState
initialState = GameState {
  snake = [(0, 0)] ,
  direction = (1, 0),
  food = initFood cantHuevos [(0,0)],
  vacio= [],--removeElements (removeElements [(a,b)| a <- [-10..10],b <- [-10..10]] [(0,0)]) (initFood cantHuevos [(0,0)]),
  gameOver = False,
  score=0
}


initFood :: Int -> [(Int,Int)] -> [(Int,Int)]
initFood cantHuevos snake = (randomFood (0,0) cantHuevos snake)

removeElements :: Eq a => [a] -> [a] -> [a]
removeElements xs ys = filter (\x -> not (elem x ys)) xs

-- Define the window size and title
window :: Display
window = InWindow "Yoan-Kevin Snake" (640, 480) (10, 10)

cantHuevos :: Int
cantHuevos = 5

verdeOscuro :: Color
verdeOscuro = makeColor 0.1 0.5 0.1 1.0

-- Define the background color
backgroundColor :: Color
backgroundColor = makeColorI 0 0 0 255

-- Define the snake color
snakeColor :: Color
snakeColor = makeColorI 0 255 0 255

-- Define the food color
foodColor :: Color
foodColor = makeColorI 255 0 0 255

-- Define the size of each cell in the grid
cellSize :: Float
cellSize = 20.0


randomFood :: (Int,Int) -> Int -> [(Int,Int)] -> [(Int,Int)]
randomFood _ 0 _ = []
randomFood (x, y) n invalids = 
  if (x', y') `elem` invalids 
    then randomFood (x', y') n invalids 
  else 
    (x', y'):(randomFood (x,y) (n-1) ((x',y'):invalids))
  where
    x1=randomRIO (-10, 10) :: IO Int
    y1=randomRIO (-10, 10) :: IO Int
    x'= unsafePerformIO x1
    y'= unsafePerformIO y1

updateFood :: [(Int,Int)] -> (Int,Int) -> Int -> [(Int,Int)] -> [(Int,Int)]
updateFood food (x,y) n invalids=
  if( length food == 1) then randomFood (x,y) n invalids
  else delete (x,y) food

-- Define the update function
update :: Float -> GameState -> GameState
update _ gameState@(GameState { snake = (x, y):xs, direction = (dx, dy), food = food, gameOver = False ,score=score}) =
    if x' < -10 || x' > 10 || y' < -10 || y' > 10 || (x', y') `elem` xs
        then gameState { gameOver = True }
    else if (x', y') `elem` food
        then gameState { snake = (x', y'):  (x,y):xs, food = updateFood food (x', y') cantHuevos (((x',y'):(x,y):xs)++food) ,score=score+1}
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
render GameState { snake = (x,y):xs, food = food, gameOver = False ,score=score, vacio=vacio} =
  pictures [renderSnake xs , renderFood food , renderScore score ,renderVacias vacio, renderHead (x,y) ]
  where
    renderHead (x,y) = color verdeOscuro (renderCell (x,y))
    renderSnake xs = color snakeColor (pictures (map renderCell xs))
    renderFood food = color foodColor (pictures (map renderCell food))
    renderVacias vacio = color white (pictures (map renderCell vacio))
    renderScore score = translate (-300) 200 (scale 0.2 0.2 (color red (text ("Score: "++(show score)))))
    renderCell (x, y) = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (rectangleSolid cellSize cellSize)
render GameState { gameOver = True } =
  translate (-200) 0 (scale 0.5 0.5 (color red (text "Game Over")))
  
-- Define the main function
main :: IO ()
main = play window backgroundColor 10 initialState render handleInput update