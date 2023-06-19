module Generator1 where

import Base
import Data.List (transpose)
import System.Random (randomRs, newStdGen)


generateBoard :: Int -> Int -> Int -> StdGen -> Board
generateBoard rows columns  mines gen =
    cellToBoard columns $ generateNumbers columns $ genereateMines rows columns mines gen

--Genera posiciones aleatorias para las minas actualiza los numeros de las celdas para generar el tablero
generateMines :: Int -> Int -> Int -> StdGen -> [Typecell]
generateMines rows cols mines gen =
    let positionsMines = take mines $ nub $ randomRs(0, rows * cols - 1) gen 
        typeCells = [if i `elem` positionsMines then Mine else Empty | i <- [0..rows*cols-1]]
      --cells = zip  typeCells take rows * cols $ repeat Covered 


-- Genera los números que indican las minas adyacentes a una celda vacía
generateNumbers :: Int -> [TypeCell] -> [TypeCell]
generateNumbers columns cells = [putNumber cells i columns | i <- [0..length cells - 1]]

-- Genera el número para una celda vacía
putNumber :: [TypeCell]-> Int -> Int -> TypeCell
putNumber cells index columns = if cells !! index == Empty then Number $ countMines cells index columns else cells !! index 


-- Cuenta las minas adyacentes a una celda (Celda de mina)
countMines :: [TypeCell] -> Int -> Int -> Int
countMines cells i columns = length $ getAdjacentMines cells i columns

-- | Obtiene las minas adyacentes a una celda en el tablero
--
-- Recibe una lista de celdas(Representacion lineal del tablero) el indice de la celda y la cantidad de columnas para 
-- poder calcular el indice correspondiente de la posicion adyacente en la lista lineal  
getAdjacentMines :: [TypeCell] -> Int -> Int -> [TypeCell]
getAdjacentMines cells index columns = filter (== Mine) $ map (\(i, j) -> cells !! index + i*columns + j) adjVectors
  where
    --una lista que da todas las posibles combinaciones de los vectores de adyacencia
    adjVectors = [(i, j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]


-- Función para dado una lista de celdas convertirlo en un tablero(lista de Celdas)
cellToBoard :: Int -> [TypeCell] -> Board
cellToBoard _ [] = Nothing
-- mediante el zip se convierte la lista de typeCells en una lista de Celdas (Una dupla (TypeCell, Status))
cellToBoard n xs = zip $ take n xs $ take n repeat Covered  : cellToBoard n (drop n xs)


