import Data.List

data Ship = Patrol | Destroyer | Submarine | Battleship | Carrier
    deriving (Eq, Ord)

data Orientation = Horizontal | Vertical

type Cell = Maybe Ship

type Board = [[Cell]]

type GridRef = (Int, Int)

instance Show Ship where
    show Patrol = "P"
    show Destroyer = "D"
    show Submarine = "S"
    show Battleship = "B"
    show Carrier = "C"

prettyBoard :: Board -> String
prettyBoard = unlines . map prettyRow

prettyRow :: [Cell] -> String
prettyRow = unwords . map prettyCell

prettyCell :: Cell -> String
prettyCell Nothing = "·"
prettyCell (Just a) = show a

-------------

replaceRow :: Int -> ([Cell] -> [Cell]) -> Board -> Board
replaceRow rowNum trans board = map (\(row, i) ->
        if i == rowNum then trans row else row
    ) $ zip board [0..]

replaceCellsForRow :: Int -> Int -> Cell -> [Cell] -> [Cell]
replaceCellsForRow 0 _ _ row = row
replaceCellsForRow count start newCell row =
    [ if inRange start (count + start) i then newCell else cell
      | (cell, i) <- zip row [0..] ]

shipSize :: Ship -> Int
shipSize Patrol = 2
shipSize Destroyer = 3
shipSize Submarine = 3
shipSize Battleship = 4
shipSize Carrier = 5

newBoard :: [[Cell]]
newBoard = replicate 10 $ replicate 10 Nothing

placeShip :: Ship -> Orientation -> GridRef -> Board -> Board
placeShip ship Horizontal grid board =
    placeHorizontal (Just ship) (shipSize ship) grid board
placeShip ship Vertical grid board =
    placeVertical (Just ship) (shipSize ship) grid board

placeHorizontal :: Cell -> Int -> GridRef -> Board -> Board
placeHorizontal cell size (row, col) board  = replaceRow row placer board
    where placer = replaceCellsForRow size col cell

placeVertical :: Cell -> Int -> GridRef -> Board -> Board
placeVertical cell size (row, col) board =
    [ if inRange row (row + size) i then placer rowCells else rowCells
      | (rowCells, i) <- zip board [0..] ]
    where placer = replaceCellsForRow 1 col cell

inRange :: Int -> Int -> Int -> Bool
inRange lo hi v = lo <= v && v < hi

-------------

board = foldl' (\o f -> f o) newBoard [
          placeShip Destroyer Horizontal (0, 0)
        , placeShip Patrol Horizontal (2, 2)
        , placeShip Patrol Horizontal (9, 6)
        , placeShip Submarine Horizontal (0, 5)
        , placeShip Submarine Vertical (4, 2)
        , placeShip Battleship Horizontal (7, 0)
        , placeShip Carrier Vertical (3, 9)
    ]

main :: IO ()
main = putStrLn $ prettyBoard board

