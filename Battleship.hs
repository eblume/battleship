import Data.List

data Ship = Patrol | Destroyer | Submarine | Battleship | Carrier
    deriving (Eq, Ord)
data Orientation = Horizontal | Vertical
type Cell = Maybe Ship
type Board = [[Cell]]
type GridRef = (Int, Int)
data Placement = Placement Ship Orientation GridRef
data HitMiss = Hit | Miss | Empty
type HitBoard = [[HitMiss]]

instance Show Ship where
    show Patrol = "P"
    show Destroyer = "D"
    show Submarine = "S"
    show Battleship = "B"
    show Carrier = "C"

instance Show HitMiss where
    show Hit = "+"
    show Miss = "~"
    show Empty = "."

prettyBoard :: Board -> String
prettyBoard = unlines . map prettyRow

prettyRow :: [Cell] -> String
prettyRow = unwords . map prettyCell

prettyCell :: Cell -> String
prettyCell Nothing = "·"
prettyCell (Just a) = show a

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

------------

newBoard :: Board
newBoard = replicate 10 $ replicate 10 Nothing

makeBoard :: [Placement] -> Board
makeBoard = foldl' placeShip $ newBoard

placeShip :: Board -> Placement -> Board
placeShip board (Placement ship orient gridref) =
    f (Just ship) (shipSize ship) gridref board
  where f = case orient of Horizontal -> placeHorizontal
                           Vertical   -> placeVertical

placeHorizontal :: Cell -> Int -> GridRef -> Board -> Board
placeHorizontal cell size (row, col) = replaceRow row placer
    where placer = replaceCellsForRow size col cell

placeVertical :: Cell -> Int -> GridRef -> Board -> Board
placeVertical cell size (row, col) board =
    [ if inRange row (row + size) i then placer rowCells else rowCells
      | (rowCells, i) <- zip board [0..] ]
    where placer = replaceCellsForRow 1 col cell

inRange :: Int -> Int -> Int -> Bool
inRange lo hi v = lo <= v && v < hi

-------------

prettyHitBoard :: HitBoard -> String
prettyHitBoard = unlines . map prettyHitRow

prettyHitRow :: [HitMiss] -> String
prettyHitRow = unwords . map show

-- Overlay hits and misses on a game board
maskBoard :: HitBoard -> Board -> String
maskBoard hb b = unlines . map maskRow $ (zip hb b)

maskRow :: ([HitMiss], [Cell]) -> String
maskRow (hitrow, row) = unwords . map maskCell $ (zip hitrow row)

maskCell :: (HitMiss, Cell) -> String
maskCell (Empty, cell) = prettyCell cell
maskCell (hm, _) = show hm
 
-------------

board = makeBoard [
    Placement Destroyer Horizontal (0, 0)
  , Placement Patrol Horizontal (2, 2)
  , Placement Patrol Horizontal (9, 6)
  , Placement Submarine Horizontal (0, 5)
  , Placement Submarine Vertical (4, 2)
  , Placement Battleship Horizontal (7, 0)
  , Placement Carrier Vertical (3, 9)
    ]

-- just for debugging
boardToHitBoard :: Board -> HitBoard
boardToHitBoard board = [[ case cell of Nothing -> Empty
                                        Just _  -> Hit
                           | cell <- row ]
                           | row <- board ]


main :: IO ()
main = putStrLn $ maskBoard (boardToHitBoard board) board

