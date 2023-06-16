import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.List
import qualified Data.Map as Map
import System.IO.Unsafe
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

-- Define the game state
data GameState = GameState {
  obstaculos :: [(Int,Int)],
  grid :: [[Cell]],
  snake :: [(Int, Int)],
  direction :: (Int, Int),
  food :: [((Int, Int),Int)],
  gameOver :: Bool,
  playMode :: Bool,
  score :: Int
}


type Queue a = [a]
data Cell = Empty | Blocked deriving (Eq, Show)

isObstacle :: [[Cell]] -> (Int, Int) -> Bool
isObstacle matrix (x, y) = ((matrix !! x) !! y) == Blocked

isValidMapa obstaculos start grid = ((length obstaculos) + (length (bfs grid start))) == 20 

--Ok

bfs :: [[Cell]] -> (Int, Int) -> [((Int, Int), Int)]
bfs matrix start = bfs' [(start, 0)] []
  where
    bfs' :: Queue ((Int, Int), Int) -> [((Int, Int), Int)] -> [((Int, Int), Int)]
    bfs' [] visited = visited
    bfs' ((pos, dist):queue) visited
      | pos `elem` map (\(p, _) -> p) visited = bfs' queue visited
      | otherwise =
        let neighbors = filter (\p -> not (isObstacle matrix p)) (getNeighbors pos)
            newQueue = queue ++ map (\p -> (p, dist + 1)) neighbors
            newVisited = visited ++ [(pos, dist)]
        in bfs' newQueue newVisited

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) = [((x-1+20)`mod`20, y), ((x+1+20)`mod`20, y), (x, (y-1+20)`mod`20), (x, (y+1+20)`mod`20)]

-- Ok

updateMatrix :: a -> [(Int, Int)] -> [[a]] -> [[a]]
updateMatrix val points matrix = foldl (\acc (i,j) -> update2D i j val acc) matrix points
  where
    update2D i j val xs = take i xs ++ [updateRow j val (xs !! i)] ++ drop (i+1) xs
    updateRow j val row = take j row ++ [val] ++ drop (j+1) row
--Ok 
-- Define the initial game state
initialState :: [(Int,Int)] -> GameState
initialState obstaculos = GameState {
  gameOver = False,
  obstaculos = obstaculos,
  grid = updateMatrix Blocked obstaculos (replicate 20 (replicate 20 Empty)),
  snake = randomItems (0,0) 1 obstaculos  ,
  food = zip (randomItems (0,0) cantHuevos ((snake (initialState obstaculos))++obstaculos)) (cycle [1..cantHuevos]),
  direction = (1, 0),
  playMode=True,
  score=0
}

obstaculosRandom :: Int -> [(Int,Int)]
obstaculosRandom n = randomItems (0,0) 7 [] 

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


randomItems :: (Int,Int) -> Int -> [(Int,Int)] -> [(Int,Int)]
randomItems _ 0 _ = []
randomItems (x, y) n invalids = 
  if (x', y') `elem` invalids 
    then randomItems (x', y') n invalids 
  else 
    (x', y'):(randomItems (x,y) (n-1) ((x',y'):invalids))
  where
    x1=randomRIO (0, 19) :: IO Int
    y1=randomRIO (0, 19) :: IO Int
    x'= unsafePerformIO x1
    y'= unsafePerformIO y1

updateFood :: [((Int,Int),Int)] -> (Int,Int) -> Int -> [(Int,Int)] -> [((Int,Int),Int)]
updateFood food (x,y) n invalids=
  if( length food == 1) then (zip (randomItems (0,0) cantHuevos invalids) (cycle [1..cantHuevos]))
  else filter (\((a,b),z) -> a/=x||b/=y) food

updateScore :: Int -> Map.Map (Int,Int) Int -> [((Int,Int),Int)] -> Int
updateScore score matrix food = score+100*(maximum $ snd $ unzip $ huevosQueCumpleUno (findMin food matrix) food matrix)

myDic :: Ord k => [(k, v)] -> Map.Map k v
myDic pares = Map.fromList pares

findMin :: [((Int,Int),Int)] -> Map.Map (Int,Int) Int -> Int
findMin [] _  = 25000
findMin (((x,y),z):xs) matrix  = min (matrix Map.! (x,y)) (findMin xs matrix)

huevosQueCumpleUno :: Int -> [((Int,Int),Int)] -> Map.Map (Int,Int) Int -> [((Int,Int),Int)]
huevosQueCumpleUno mini food matrix = filter (\((x,y),z) -> (matrix Map.! (x,y)) == mini) food




-- Define the update function
update :: Float -> GameState -> GameState
update _ gameState@(GameState { obstaculos = obstaculos ,snake = (x, y):xs, direction = (dx, dy), food = food, gameOver = False ,score=score, grid=grid}) =
    if (x', y') `elem` xs || (x', y') `elem` obstaculos
        then gameState { gameOver = True }
    else if (x', y') `elem` fst (unzip food)
        then gameState { snake = (x', y'):  (x,y):xs, food = updateFood food (x', y') cantHuevos (((x',y'):(x,y):xs)++obstaculos) ,score=if length food /= 1 then updateScore score (myDic (bfs grid (x',y'))) (filter (\((a,b),z) -> a/=x'||b/=y') food) else score}
    else 
        gameState { snake = (x', y') : init ((x,y):xs) }
    where
        x' = (x + 20 + dx) `mod` 20
        y' = (y + 20 + dy) `mod` 20      
update _ gameState = gameState

-- Define the input handling function
isOpposite :: (Int, Int) -> (Int, Int) -> Bool
isOpposite (x1, y1) (x2, y2) = x1 == -x2 && y1 == -y2

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (MouseButton LeftButton) Down _ (x,y)) gameState@(GameState{obstaculos=obstaculos, playMode=False})=
  gameState{obstaculos=final}
  where
    x'=(round (x/20)) + 10
    y'=(round (y/20)) + 10
    final = if (x',y') `elem` obstaculos then (delete (x',y') obstaculos) else ((x',y'):obstaculos)
handleInput (EventKey (Char 'p') _ _ _) gameState@(GameState { playMode=False, obstaculos=obstaculos}) = initialState obstaculos 
handleInput (EventKey (Char 'n') _ _ _) gameState@(GameState { gameOver=True}) = gameState { playMode=False }
handleInput (EventKey (Char 'r') _ _ _) gameState@(GameState { gameOver=True, obstaculos=obstaculos}) = initialState obstaculos
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
renderObstaculos obstaculos= color blue (pictures (map renderCell obstaculos))
renderObstaculosB obstaculos= color darkBlue (pictures (map renderCellB obstaculos))
renderCeldas = color black (pictures (map renderCell mapa))
renderCeldasB = color gray (pictures (map renderCellB mapa))
renderCell (x, y) = translate (fromIntegral (x-10) * cellSize) (fromIntegral (y -10) * cellSize) (rectangleSolid cellSize cellSize)
renderCellB (x, y) = translate (fromIntegral (x-10) * cellSize) (fromIntegral (y -10) * cellSize) (rectangleWire cellSize cellSize)
mapa= [(a,b)| a <- [0..19],b <- [0..19]]

-- Define la función roundedRectangle para dibujar un rectángulo con bordes suaves
-- | Color azul oscuro
darkBlue :: Color
darkBlue = makeColor 0.1 0.1 0.5 1.0


-- | Crea un color gris con una intensidad dada
gray :: Color
gray = makeColor 0.1 0.1 0.1 1.0



-- | Crea un color rojo oscuro con una intensidad dada
darkRed :: Color
darkRed = makeColor 0.5 0.1 0.1 1.0

render :: GameState -> Picture
render GameState {obstaculos=obstaculos, snake = (x,y):xs, food = food, gameOver = False ,score=score} =
  pictures [renderCeldas,renderCeldasB,renderObstaculos obstaculos,renderObstaculosB obstaculos,renderSnake xs ,renderSnakeB xs, renderFood (fst (unzip food)),renderFoodB (fst (unzip food)), renderFoodVerde (fst (unzip food)), renderHead (x,y) ,renderNumeros food,renderScore score]
  where
    renderHead (x,y) = color verdeOscuro (renderCell (x,y))
    renderSnake xs = color green (pictures (map renderCell xs))
    renderSnakeB xs = color verdeOscuro (pictures (map renderCellB xs))
    renderNumeros food = color white (pictures (map renderCellNumeroFood food))
    renderFood food = color red (pictures (map renderCell food))
    renderFoodB food = color darkRed (pictures (map renderCellB food))
    renderFoodVerde food = color verdeOscuro (pictures (map renderCellV food))
    renderScore score = translate (-310) 215 (scale 0.15 0.15 (color red (text ("Score: "++(show score)))))
    renderCellNumeroFood ((x,y),z) = (translate (fromIntegral (x-10) * cellSize) (fromIntegral (y -10) * cellSize) (scale 0.18 0.18 (text (show z))))
    renderCellV (x, y) = translate (fromIntegral (x-10) * cellSize) (fromIntegral (y -10) * cellSize) (rectangleSolid (cellSize/3) (cellSize/3))
render GameState { gameOver = True ,playMode=True, score=score} = pictures
  [translate (-200) 0 (scale 0.5 0.5 (color red (text ("Game Over")))),
  translate (-135) (-90) (scale 0.4 0.4 (color red (text ("Score: "++(show score)))))]
render GameState { gameOver = True, playMode=False ,obstaculos=obstaculos } = 
  pictures [renderCeldas,renderCeldasB,renderObstaculos obstaculos,renderObstaculosB obstaculos]
  
-- Define the main function
main :: IO ()
main = do
  play window white 6 (initialState (obstaculosRandom 7))render handleInput update
  