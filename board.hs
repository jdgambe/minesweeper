module Board where

getCell :: Board -> Position -> Cell
getCell board (x, y) = board !! x !! y


getTypeCell :: Cell -> TypeCell
getTypeCell (type, _) = type

getStatusCell :: Cell -> Status
getStatusCell (_, stat) = stat

-- |Funcion que dada la posicion (x, y) descubre la celda y actualiza los adyacentes
uncoverCell :: Board -> Position -> Board
uncoverCell  board (x, y) =
    case getTypeCell (getCell board (x, y)) of
        Empty -> updateEmptyCells (x, y) board
        Number _ -> updateStatus board (x, y) Uncovered
        _ -> updateMinesCells board

-- |Funcion que recursivamente descubre todas las celdas vacias al descubrir alguna
updateEmptyCells :: Board -> Position -> Board
updateEmptyCells board (x, y) =
    let cell = getCell board (x, y)
      updatedBoard = updateStatus board (x, y) Uncovered
    in if getTypeCell cell == Empty && getStatus cell == Covered
       then foldr updateEmptyCells updatedBoard (getAdjacentPositions (x, y))
       else updatedBoard


updateMinesCells :: Board -> Board
updateMinesCells board =
    map (map(\cell@(typeCells, _)-> if typeCells == Mine then (Mine, Uncovered) else cell)) board
   
countUncoveredCells :: Board -> Int
countUncoveredCells board = length $ filter (\(_, status) -> status == Uncovered) $ concat board

-- Funcion para cambiar el estado de una celda del Tablero
updateStatus :: Board -> Position -> Status -> Board
updateStatus board (x, y) status = 
    take x board ++ [ take y (board !! x) ++ [((board !! x !! y), status)]++ drop(y + 1) (board !! x)] ++ 
    drop (x + 1) board
