import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Define the game state
data GameState = GameState {
                   snake :: [(Int, Int)],
                   direction :: (Int, Int),
                   food :: (Int, Int),
                   gameOver :: Bool
                 }

-- Define the initial game state
initialState :: GameState
initialState = GameState {
                 snake = [(0, 0)],
                 direction = (1, 0),
                 food = (5, 5),
                 gameOver = False
               }

-- Define the window size and title
window :: Display
window = InWindow "Yoan-Kevin Snake" (640, 480) (10, 10)

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

-- Define the update function
update :: Float -> GameState -> GameState
update _ gameState@(GameState { snake = (x, y):xs, direction = (dx, dy), food = food, gameOver = False }) =
    if x' < -10 || x' > 10 || y' < -10 || y' > 10 || (x', y') `elem` xs
        then gameState { gameOver = True }
    else if (x', y') == food
        then gameState { snake = (x', y'):  (x,y):xs, food = randomFood (x', y') }
    else 
        gameState { snake = (x', y') : init ((x,y):xs) }
    where
        x' = x + dx
        y' = y + dy
        randomFood (x, y) = 
            if (x', y') `elem` xs 
                then randomFood (x, y) 
            else 
                (x', y')
            where
            x' = fst $ randomR (-10, 10) (mkStdGen (round (fromIntegral x * fromIntegral y * 1000)))
            y' = fst $ randomR (-10, 10) (mkStdGen (round (fromIntegral x * fromIntegral y * 1000)))
--update _ gameState@(GameState { snake = [], gameOver = False }) = gameState
update _ gameState = gameState

-- Define the input handling function
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'w') _ _ _) gameState@(GameState { direction = (dx, dy) }) =
  if dy /= -1 then gameState { direction = (0, 1) } else gameState
handleInput (EventKey (Char 'a') _ _ _) gameState@(GameState { direction = (dx, dy) }) =
  if dx /= 1 then gameState { direction = (-1, 0) } else gameState
handleInput (EventKey (Char 's') _ _ _) gameState@(GameState { direction = (dx, dy) }) =
  if dy /= 1 then gameState { direction = (0, -1) } else gameState
handleInput (EventKey (Char 'd') _ _ _) gameState@(GameState { direction = (dx, dy) }) =
  if dx /= -1 then gameState { direction = (1, 0) } else gameState
handleInput _ gameState = gameState

-- Define the rendering function
render :: GameState -> Picture
render gameState@(GameState { snake = snake, food = food, gameOver = False }) =
  pictures $ renderSnake snake : renderFood food : []
  where
    renderSnake = color snakeColor . pictures . map renderCell
    renderFood = color foodColor . renderCell
    renderCell (x, y) = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ rectangleSolid cellSize cellSize
render gameState@(GameState { gameOver = True }) =
  translate (-200) 0 $ scale 0.5 0.5 $ color red $ text "Game Over"
  
-- Define the main function
main :: IO ()
main = play window backgroundColor 10 initialState render handleInput update