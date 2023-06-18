{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.List
import GHC.Generics
import System.Directory
import qualified Data.Map as Map
import Data.Aeson
import System.IO.Unsafe
import System.IO
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import qualified Data.ByteString.Lazy as B

-- Define the game state
data GameState = GameState {
  obstaculos :: [(Int,Int)],
  largoGrid :: Int,
  anchoGrid :: Int,
  grid :: [[Cell]],
  cantHuevos :: Int,
  snake :: [(Int, Int)],
  direction :: (Int, Int),
  food :: [((Int, Int),Int)],
  gameOver :: Bool,
  teclaPresionada :: Bool,
  playMode :: Bool,
  pathSL :: String,
  score :: Int
} deriving (Generic,Show)

instance ToJSON GameState
instance ToJSON Cell

instance FromJSON GameState
instance FromJSON Cell

type Queue a = [a]
data Cell = Empty | Blocked deriving (Eq, Show, Generic)

isObstacle :: [[Cell]] -> (Int, Int) -> Bool
isObstacle matrix (x, y) = ((matrix !! x) !! y) == Blocked

isValidMapa obstaculos grid = if (length obstaculos == ((length grid) * (length (head grid)))) then False else ((length obstaculos) + (length (bfs grid (head (randomItems (0,0) 1 obstaculos (length grid) (length (head grid))))))) == ((length grid) * (length (head grid)))

--Ok

bfs :: [[Cell]] -> (Int, Int) -> [((Int, Int), Int)]
bfs matrix start = bfs' [(start, 0)] []
  where
    bfs' :: Queue ((Int, Int), Int) -> [((Int, Int), Int)] -> [((Int, Int), Int)]
    bfs' [] visited = visited
    bfs' ((pos, dist):queue) visited
      | pos `elem` map (\(p, _) -> p) visited = bfs' queue visited
      | otherwise =
        let neighbors = filter (\p -> not (isObstacle matrix p)) (getNeighbors pos (length matrix) (length (head matrix)) )
            newQueue = queue ++ map (\p -> (p, dist + 1)) neighbors
            newVisited = visited ++ [(pos, dist)]
        in bfs' newQueue newVisited

getNeighbors :: (Int, Int) -> Int -> Int -> [(Int, Int)]
getNeighbors (x, y) largo ancho = [((x-1+largo)`mod`largo, y), ((x+1+largo)`mod`largo, y), (x, (y-1+ancho)`mod`ancho), (x, (y+1+ancho)`mod`ancho)]

-- Ok

updateMatrix :: a -> [(Int, Int)] -> [[a]] -> [[a]]
updateMatrix val points matrix = foldl (\acc (i,j) -> update2D i j val acc) matrix points
  where
    update2D i j val xs = take i xs ++ [updateRow j val (xs !! i)] ++ drop (i+1) xs
    updateRow j val row = take j row ++ [val] ++ drop (j+1) row
--Ok 
-- Define the initial game state
initialState :: Int -> Int -> Int -> [(Int,Int)] -> GameState
initialState cantHuevos largo ancho obstaculos = GameState {
  gameOver = False,
  obstaculos = obstaculos,
  cantHuevos = cantHuevos,
  largoGrid =largo,
  anchoGrid = ancho,
  teclaPresionada=False,
  grid = updateMatrix Blocked obstaculos (replicate largo (replicate ancho Empty)),
  snake = randomItems (0,0) 1 obstaculos largo ancho ,
  food = zip (randomItems (0,0) cantHuevos ((snake (initialState cantHuevos largo ancho obstaculos))++obstaculos) largo ancho) (cycle [1..cantHuevos ]),
  direction = (1, 0),
  playMode=True,
  pathSL="<press any key>",
  score=0
}

obstaculosRandom :: Int -> Int -> Int -> [(Int,Int)]
obstaculosRandom n largo ancho = randomItems (0,0) n [] largo ancho

removeElements :: Eq a => [a] -> [a] -> [a]
removeElements xs ys = filter (\x -> not (elem x ys)) xs

-- Define the window size and title
window :: Display
window = InWindow "Yoan-Kevin Snake" (640, 480) (10, 10)

cantObstaculos :: Int
cantObstaculos = 7

verdeOscuro :: Color
verdeOscuro = makeColor 0.1 0.5 0.1 1.0

-- Define the size of each cell in the grid
cellSize :: Float
cellSize = 20.0


randomItems :: (Int,Int) -> Int -> [(Int,Int)] -> Int -> Int -> [(Int,Int)]
randomItems _ 0 _ _ _ = []
randomItems (x, y) n invalids largo ancho= 
  if (x', y') `elem` invalids 
    then randomItems (x', y') n invalids largo ancho 
  else 
    (x', y'):(randomItems (x,y) (n-1) ((x',y'):invalids) largo ancho)
  where
    x1=randomRIO (0, largo-1) :: IO Int
    y1=randomRIO (0, ancho-1) :: IO Int
    x'= unsafePerformIO x1
    y'= unsafePerformIO y1

updateFood :: [((Int,Int),Int)] -> (Int,Int) -> Int -> Int ->Int -> [(Int,Int)] -> [((Int,Int),Int)]
updateFood food (x,y) cantHuevos largo ancho invalids=
  if( length food == 1) then (zip (randomItems (0,0) cantHuevos invalids largo ancho) (cycle [1..cantHuevos]))
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
update _ gameState@(GameState { obstaculos = obstaculos ,snake = (x, y):xs, direction = (dx, dy), food = food, gameOver = False ,score=score, grid=grid, cantHuevos=cantHuevos, largoGrid=largoGrid,anchoGrid=anchoGrid}) =
    if (x', y') `elem` (x,y):xs || (x', y') `elem` obstaculos || ((largoGrid*anchoGrid)-(length obstaculos)-(length ((x,y):xs))<cantHuevos)
        then gameState { gameOver = True }
    else if (x', y') `elem` fst (unzip food)
        then gameState { teclaPresionada=False,snake = (x', y'):  (x,y):xs, food = updateFood food (x', y') cantHuevos largoGrid anchoGrid (((x',y'):(x,y):xs)++obstaculos) ,score=if length food /= 1 then updateScore score (myDic (bfs grid (x',y'))) (filter (\((a,b),z) -> a/=x'||b/=y') food) else score}
    else 
        gameState { teclaPresionada=False,snake = (x', y') : init ((x,y):xs) }
    where
        x' = (x + largoGrid + dx) `mod` largoGrid
        y' = (y + anchoGrid + dy) `mod` anchoGrid 

update _ gameState = gameState

-- Define the input handling function
isOpposite :: (Int, Int) -> (Int, Int) -> Bool
isOpposite (x1, y1) (x2, y2) = x1 == -x2 && y1 == -y2

divCeiling :: (Integral a) => a -> a -> a
divCeiling x y = ceiling ((fromIntegral x) / (fromIntegral y))

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (MouseButton LeftButton) Down _ (x,y)) gameState@(GameState { playMode=False ,cantHuevos=cantHuevos,obstaculos=obstaculos,largoGrid=largoGrid,anchoGrid=anchoGrid,pathSL=pathSL}) = 
  if isInsideButton buttonPlaym  (x,y) then 
    (if esValido then classic else gameState{pathSL=invalido}) 
  else if isInsideButton buttonSavem (x,y) then 
    (if esValido then 
      let json = encode gameState
      in unsafePerformIO(B.writeFile pathSL json) `seq` gameState{pathSL=saved}
      else gameState{pathSL=invalido}
    )
  else if isInsideButton buttonLoadm (x,y) then (
    if unsafePerformIO(doesFileExist pathSL) then do
    let fileContent = unsafePerformIO(B.readFile pathSL)
    let eitherGame = eitherDecode fileContent :: Either String GameState
    case eitherGame of 
      Right estado -> estado {pathSL=loaded}
      Left error -> gameState {pathSL=err}
    else gameState{pathSL=noExiste})
  else if isInsideButton buttonUp1 (x,y) then gameState {anchoGrid=if anchoGrid/=20 then anchoGrid+1 else anchoGrid ,obstaculos=[]} 
  else if isInsideButton buttonUp2 (x,y) then gameState {largoGrid=if largoGrid/=20 then largoGrid+1 else largoGrid, obstaculos=[]}
  else if isInsideButton buttonDown1 (x,y) then gameState {anchoGrid=if anchoGrid/=6 then anchoGrid-1 else anchoGrid, obstaculos=[]}
  else if isInsideButton buttonDown2 (x,y) then gameState {largoGrid=if largoGrid/=6 then largoGrid-1 else largoGrid, obstaculos=[]}
  else if isInsideButton buttonUp3 (x,y) then gameState {cantHuevos=if cantHuevos/=20 then cantHuevos+1 else cantHuevos}
  else if isInsideButton buttonDown3 (x,y) then gameState{cantHuevos=if cantHuevos/=0 then cantHuevos-1 else cantHuevos} 
 -- else if isInsideButton buttonUp4 (x,y) then gameState{fps=if fps/=12 then fps+1 else fps}
 -- else if isInsideButton buttonDown4 (x,y) then gameState{fps=if fps/=6 then fps-1 else fps}
  else
    gameState{obstaculos=final}
    where
      classic=initialState cantHuevos largoGrid anchoGrid obstaculos
      esValido = isValidMapa  obstaculos (updateMatrix Blocked obstaculos (replicate largoGrid (replicate anchoGrid Empty))) && ((largoGrid*anchoGrid)-(length obstaculos)-2>=cantHuevos) 
      x'=(round (x/cellSize)) + (divCeiling largoGrid 2)
      y'=(round (y/cellSize)) + (divCeiling anchoGrid 2)
      final = if x'<0 || y'<0 || x'> (largoGrid-1) || y'>(anchoGrid-1) then obstaculos else if (x',y') `elem` obstaculos then (delete (x',y') obstaculos) else ((x',y'):obstaculos)

handleInput (EventKey (Char '\b') Down _ _) gameState@(GameState {playMode=False , pathSL=pathSL})= gameState{ pathSL = if pathSL=="" || pathSL==press || pathSL==invalido || pathSL==saved || pathSL==loaded || pathSL==err || pathSL== noExiste then "" else init pathSL } 
handleInput (EventKey (Char k) Down _ _) gameState@(GameState {playMode=False , pathSL=pathSL})=gameState{ pathSL= if pathSL==press || pathSL==invalido||pathSL==saved||pathSL==loaded || pathSL==err || pathSL==noExiste then [k] else pathSL++[k]}
handleInput (EventKey (MouseButton LeftButton) Down _ (x,y)) gameState@(GameState { gameOver=True, playMode=True,obstaculos=obstaculos ,largoGrid=largoGrid,anchoGrid=anchoGrid,cantHuevos=cantHuevos}) = 
  if isInsideButton buttonPlay (x,y)   then initialState cantHuevos largoGrid anchoGrid obstaculos else if isInsideButton buttonEditor (x,y) then gameState{playMode =False} else gameState
handleInput (EventKey (Char 'w') Down _ _) gameState@(GameState { direction = dir ,teclaPresionada=False})
  | not (isOpposite dir (0, 1)) = gameState { direction = (0, 1),teclaPresionada=True }
handleInput (EventKey (Char 'a') Down _ _) gameState@(GameState { direction = dir ,teclaPresionada=False})
  | not (isOpposite dir (-1, 0)) = gameState { direction = (-1, 0),teclaPresionada=True }
handleInput (EventKey (Char 's') Down _ _) gameState@(GameState { direction = dir ,teclaPresionada=False})
  | not (isOpposite dir (0, -1)) = gameState { direction = (0, -1),teclaPresionada=True }
handleInput (EventKey (Char 'd') Down _ _) gameState@(GameState { direction = dir ,teclaPresionada=False})
  | not (isOpposite dir (1, 0)) = gameState { direction = (1, 0) ,teclaPresionada=True}
handleInput _ gameState = gameState

saved="Saved"
loaded="Loaded"
press="<press any key>"
invalido="Invalid Grid"
noExiste="Not such file"
err="ERROR"

-- | Color azul oscuro
darkBlue :: Color
darkBlue = makeColor 0.1 0.1 0.5 1.0


-- | Crea un color gris con una intensidad dada
gray :: Color
gray = makeColor 0.1 0.1 0.1 1.0

restartGame :: Int -> GameState -> IO ()
restartGame newFps gameState = play window white newFps gameState render handleInput update

-- | Crea un color rojo oscuro con una intensidad dada
darkRed :: Color
darkRed = makeColor 0.5 0.1 0.1 1.0

data Button = Button { 
  buttonPos :: (Float, Float),
  buttonSize :: (Float, Float),
  buttonLabel :: String,
  scaleB :: Float
}

isInsideButton :: Button -> (Float,Float)-> Bool
isInsideButton button (x, y) =
    let (bx, by) = buttonPos button
        (bw, bh) = buttonSize button
    in x >= bx - bw / 2 && x <= bx + bw / 2 && y >= by - bh / 2 && y <= by + bh / 2

sizeButton :: (Float,Float)
sizeButton = (150,60)

sizeButtonm :: (Float,Float)
sizeButtonm = (105,60)

sizeButtonUpDown :: (Float,Float)
sizeButtonUpDown = (40,30)

buttonBasic :: Float -> Float -> String -> Float -> (Float,Float)-> Button
buttonBasic x y s scaleB sizeButton = Button{
  buttonPos=(x,y),
  buttonSize=sizeButton,
  buttonLabel=s,
  scaleB=scaleB
}


drawButton :: Button -> Picture
drawButton button@(Button{ buttonLabel=buttonLabel, buttonPos=(x,y),buttonSize=(w,h),scaleB=scaleB}) = pictures
  [ translate x y $ color white $ rectangleSolid w h
  , translate x y $ color darkRed$ rectangleWire w h
  , translate (x - w / 2+5) (y - h / 2+10) $ color darkRed $ scale scaleB scaleB $ text buttonLabel
  ]
  where
    (x, y) = buttonPos button
    (w, h) = buttonSize button


buttonUp1 :: Button
buttonUp1 = buttonBasic (-240) (140) "Up" 0.1 sizeButtonUpDown

buttonUp2 :: Button
buttonUp2 = buttonBasic (-240) (50) "Up" 0.1 sizeButtonUpDown

buttonUp3 :: Button
buttonUp3 = buttonBasic (-240) (-40) "Up" 0.1 sizeButtonUpDown

buttonUp4 :: Button
buttonUp4 = buttonBasic (-240) (-130) "Up" 0.1 sizeButtonUpDown

buttonDown1 :: Button
buttonDown1 = buttonBasic (-240) (110) "Down" 0.1 sizeButtonUpDown

buttonDown2 :: Button
buttonDown2 = buttonBasic (-240) (20) "Down" 0.1 sizeButtonUpDown

buttonDown3 :: Button
buttonDown3 = buttonBasic (-240) (-70) "Down" 0.1 sizeButtonUpDown

buttonDown4 :: Button
buttonDown4 = buttonBasic (-240) (-160) "Down" 0.1 sizeButtonUpDown

buttonPlay :: Button
buttonPlay = buttonBasic (-230) (-180) "PLAY" 0.3 sizeButton

buttonPlaym :: Button
buttonPlaym = buttonBasic (255) (-190) "PLAY" 0.3 sizeButtonm

buttonSavem :: Button
buttonSavem = buttonBasic (255) (-10) "SAVE" 0.3 sizeButtonm

buttonLoadm :: Button
buttonLoadm = buttonBasic (255) (170) "LOAD" 0.3 sizeButtonm

buttonEditor :: Button
buttonEditor = buttonBasic (230) (-180) "EDITOR" 0.3 sizeButton


-- Define the rendering function
renderObstaculos obstaculos largo ancho= color blue (pictures (map (renderCell largo ancho) obstaculos))
renderObstaculosB obstaculos largo ancho = color darkBlue (pictures (map (renderCellB largo ancho) obstaculos))
renderCeldas largo ancho = color black (pictures (map (renderCell largo ancho) (mapa largo ancho)))
renderCeldasB largo ancho = color gray (pictures (map (renderCellB largo ancho) (mapa largo ancho)))
renderCell largo ancho (x,y) = translate (fromIntegral (x-(divCeiling largo 2)) * cellSize) (fromIntegral (y -(divCeiling ancho 2)) * cellSize) (rectangleSolid cellSize cellSize)
renderCellB largo ancho (x,y) = translate (fromIntegral (x-(divCeiling largo 2)) * cellSize) (fromIntegral (y -(divCeiling ancho 2)) * cellSize) (rectangleWire cellSize cellSize)
mapa largo ancho = [(a,b)| a <- [0..(largo-1)],b <- [0..(ancho-1)]]

render :: GameState -> Picture
render GameState {obstaculos=obstaculos,largoGrid=largoGrid,anchoGrid=anchoGrid, snake = (x,y):xs, food = food, gameOver = False ,score=score} =
  pictures [renderCeldas largoGrid anchoGrid,renderCeldasB largoGrid anchoGrid,renderObstaculos obstaculos largoGrid anchoGrid,renderObstaculosB obstaculos largoGrid anchoGrid,renderSnake xs ,renderSnakeB xs, renderFood (fst (unzip food)),renderFoodB (fst (unzip food)), renderFoodVerde (fst (unzip food)), renderHead (x,y) ,renderNumeros food,renderScore score]
  where
    renderHead (x,y) = color verdeOscuro (renderCell largoGrid anchoGrid (x,y))
    renderSnake xs = color green (pictures (map (renderCell largoGrid anchoGrid) xs))
    renderSnakeB xs = color verdeOscuro (pictures (map (renderCellB largoGrid anchoGrid) xs))
    renderNumeros food = color green (pictures (map renderCellNumeroFood food))
    renderFood food = color red (pictures (map (renderCell largoGrid anchoGrid) food))
    renderFoodB food = color darkRed (pictures (map (renderCellB largoGrid anchoGrid) food))
    renderFoodVerde food = color verdeOscuro (pictures (map renderCellV food))
    renderScore score = translate (-310) 215 (scale 0.15 0.15 (color darkRed (text ("Score: "++(show score)))))
    renderCellNumeroFood ((x,y),z) = (translate (fromIntegral (x-(divCeiling largoGrid 2)) * cellSize) (fromIntegral (y -(divCeiling anchoGrid 2)) * cellSize) (scale 0.18 0.18 (text (show z))))
    renderCellV (x, y) = translate (fromIntegral (x-(divCeiling largoGrid 2)) * cellSize) (fromIntegral (y -(divCeiling anchoGrid 2)) * cellSize) (rectangleSolid (cellSize/3) (cellSize/3))
render GameState { gameOver = True ,playMode=True, score=score} = pictures
  [translate (-180) 100 (scale 0.5 0.5 (color darkRed (text ("Game Over")))),
  translate (-135) 10 (scale 0.4 0.4 (color darkRed (text ("Score:"++(show score))))),
  drawButton buttonPlay,  
  drawButton buttonEditor]
render GameState { playMode=False ,largoGrid=largoGrid,anchoGrid=anchoGrid ,obstaculos=obstaculos ,cantHuevos=cantHuevos,pathSL=pathSL} = 
  pictures [renderCeldas largoGrid anchoGrid,renderCeldasB largoGrid anchoGrid,renderObstaculos obstaculos largoGrid anchoGrid,renderObstaculosB obstaculos largoGrid anchoGrid,
  drawButton buttonPlaym,drawButton buttonSavem,drawButton buttonLoadm, drawButton buttonUp1, drawButton buttonDown1, drawButton buttonUp2,drawButton buttonDown2,
  drawButton buttonUp3,drawButton buttonDown3,--drawButton buttonUp4,drawButton buttonDown4,
  translate (-310) 120 (scale 0.15 0.15 (color darkRed (text ("N:"++(show anchoGrid))))),
  translate (-310) 30 (scale 0.15 0.15 (color darkRed (text ("K:"++(show largoGrid))))),
  translate (-310) (-60) (scale 0.15 0.15 (color darkRed (text ("Q:"++(show cantHuevos))))),
  --translate (-310) (-150) (scale 0.15 0.15 (color darkRed (text ("V:"++(show fps))))),
  translate (-200) 200 (scale 0.2 0.2 (color darkRed (text ("Name File: "++pathSL))))]
  
-- Define the main function
main :: IO ()
main = do
  let largo=20
  let ancho=20
  let fps=10
  let inicio=(initialState 5 largo ancho (obstaculosRandom 10 largo ancho))
  play window white fps inicio{gameOver=True,playMode=False} render handleInput update
  