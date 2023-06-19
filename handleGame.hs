module HandleGame where
    
import Generator1
import Board
import System.IO
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

statusGame::StatusGame
statusGame = Yet


-- Función principal para iniciar el juego
initGame :: Int -> Int -> Int -> IO Game
initGame rows cols mines = do
  gen <- newStdGen
  let board = generateBoard rows cols mines gen
  return Game
    { rows = rows
    , cols = cols
    , mines = mines
    , board = board
    , uncoveredCells = 0 
    , timer = 0
    , statusGame = Yet 
    }


-- Juega el juego interactuando con el usuario
play :: Game -> Position -> IO Game
play game@(Game _ _ mines board uncoveredCells timer statusGame) position@(x, y) = do
    
    if statusGame == Yet then do
        startTime <- getCurrentTime
        let updatedGame = game { statusGame = Started }
        return play updatedGame position
    
    else do
        let cell@(cellType, _) = getCell board position
            updatedBoard = uncoverCell board position
            updatedGame = game {board = updatedBoard}
        case cellType of   
            Mine -> do
                endTime <- getCurrentTime
                return updatedGame { statusGame = GameOver,  timer  = endTime `diffUTCTime` startTime }
            Empty -> do
                let currentUncovered  = countUncoveredCells updatedBoard
                    updatedGame = updatedGame {uncoveredCells = currentUncovered }
            Number -> do
                let updatedgame = updatedGame{ board = updatedBoard, uncoveredCells = uncoveredCells game + 1 }
            
        when (uncoveredCells updatedGame == rows updatedGame * cols updatedGame - mines updatedGame) $ do
            endTime <- getCurrentTime
            let updatedGame = updatedGame { statusGame = GameOver,  timer  = endTime `diffUTCTime` startTime }
    
    return updatedGame


