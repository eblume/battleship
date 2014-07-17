import Data.List

data Ship = Patrol | Destroyer | Submarine | Battleship | Carrier
    deriving (Eq, Ord)

type Cell = Maybe Ship

type Board = [[Cell]]

type GridRef = (Int, Int)

-- Ship shortcut aliases
sP = Just Patrol
sD = Just Destroyer
sS = Just Submarine
sB = Just Battleship
sC = Just Carrier
nN = Nothing

instance Show Ship where
    show Patrol = "P"
    show Destroyer = "D"
    show Submarine = "S"
    show Battleship = "B"
    show Carrier = "C"

prettyBoard::Board->String
prettyBoard = unlines . map prettyRow

prettyRow::[Cell]->String
prettyRow = intercalate " " . map prettyCell

prettyCell::Cell->String
prettyCell Nothing = "·"
prettyCell (Just a) = show a

-------------


-------------

row1 = [nN, nN, sD, sD, sD, nN, nN, sS, sS, sS]
row2 = [nN, nN, nN, nN, nN, nN, nN, sB, nN, nN]
row3 = [nN, nN, nN, nN, nN, nN, nN, sB, nN, sC]
row4 = [nN, nN, nN, sS, nN, nN, nN, sB, nN, sC]
row5 = [nN, nN, nN, sS, nN, nN, nN, sB, nN, sC]
row6 = [nN, nN, nN, sS, nN, nN, nN, nN, nN, sC]
row7 = [sP, nN, nN, nN, nN, nN, nN, nN, nN, sC]
row8 = [sP, nN, nN, nN, nN, nN, nN, nN, nN, sC]
row9 = [nN, nN, nN, nN, nN, nN, nN, nN, nN, nN]
row10 = [nN, nN, sP, sP, nN, nN, nN, nN, nN, nN]

board = [row1, row2, row3, row4, row5, row6, row7, row8, row9, row10]

main :: IO ()
main = putStrLn $ prettyBoard board

