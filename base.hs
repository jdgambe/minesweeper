module Base where

data Status = Covered | Uncovered | Flagged deriving Eq
data TypeCell = Mine | Empty | Number Int deriving Eq
type Position = (Int, Int)
type Cell = (TypeCell, Status)
type Board = [[Cell]]

data StatusGame = Yet | Started | GameOver deriving Eq

data Game = Game
  { rows :: Int
  , cols :: Int
  , mines :: Int
  , board :: Board
  , uncoveredCells :: Int
  , timer :: Double
  , statusGame :: StatusGame
  }