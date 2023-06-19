import Data.Char (isDigit, toLower)


main :: IO()
main  = do
    putStrLn "Presione:"
    putStrLn "  - p para el nivel Principiante (8x8, 10 minas)"
    putStrLn "  - i para el nivel Intermedio (16x16, 40 minas)"
    putStrLn "  - e para el nivel Experto (16x30, 99 minas)"
    putStrLn "Ingrese su opción: "
    hFlush stdout
    option <- getChar
    putStrLn ""
    case option of
        'p' -> do currentGame <- initGame 8 8 10
        'i' -> do currentGame <-initGame 16 16 40
        'e' -> do currentGame <-initGame 16 30 99
        'q'-> do putStrLn "Saliendo del juego." return  
    gameLoop currentGame




playGame :: Board -> IO ()
playGame board = do
  putStrLn "¡Bienvenido al buscaminas!"
  putStrLn "Para jugar, ingresa las coordenadas de la celda que quieres descubrir."
  putStrLn "Por ejemplo, para descubrir la celda en la posición (2,3), ingresa '2 3'."
  putStrLn "Si crees que una celda contiene una mina, puedes marcarla con una bandera ingresando 'f' seguido de las coordenadas."
  putStrLn "¡Buena suerte!" 
  gameLoop board

gameLoop :: Game -> IO ()
gameLoop currGame@(rows cols mines board unc timer stat) = do
  printBoard board
  putStrLn "Ingresa las coordenadas de la celda que quieres descubrir (por ejemplo, '2 3'):"
  input <- getLine
  case parseInput input of
    Nothing -> do
        putStrLn "Entrada inválida. Inténtalo de nuevo."
        return gameLoop currGame
    Just (x, y) ->
        if isValidPosition (x, y) board then do
            currGame <- playGame currGame (x, y)
            if stat == GameOver then do
                if rows * cols - unc == mines then do
                    putStrLn "¡Felicidades Has Ganado!"
                     
                else do
                    putStrLn "¡Has encontrado una mina!"
                
                putStrLn "Fin del juego. Tiempo: "++ timer 
                return 
            
            return gameLoop currGame
        else do
          putStrLn "Posición inválida. Inténtalo de nuevo."
          return gameLoop currGame

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "Tablero actual:"
  let rows = length board
  let cols = length (head board)
  let indices = [0..rows-1]
  putStrLn $ "  " ++ unwords (map show indices)
  mapM_ (printRow board) (zip indices board)

printRow :: Board -> (Int, [Cell]) -> IO ()
printRow board (rowIndex, cells) = do
  let rowString = map (cellToString rowIndex) cells
  putStrLn $ show rowIndex ++ " " ++ unwords rowString

cellToString :: Int -> Cell -> String
cellToString rowIndex (typeCell, status)
  | status == Covered = "-"
  | status == Flagged = "F"
  | status == Uncovered =
    case typeCell of
      Mine -> "X"
      Empty -> " "
      Number n -> show n

parseInput :: String -> Maybe Position
parseInput input =
  let tokens = words input
      parsedTokens = map parseInt tokens
  in case parsedTokens of
       [Just x, Just y] -> Just (x, y)
       _ -> Nothing

parseInt :: String -> Maybe Int
parseInt str =
  if all isDigit str
    then Just (read str)
    else Nothing

isValidPosition :: Position -> Board -> Bool
isValidPosition (x, y) board =
  let rows = length board
      cols = length (head board)
  in x >= 0 && x < rows && y >= 0 && y < cols

