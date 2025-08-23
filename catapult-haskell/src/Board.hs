module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char ( digitToInt, ord, chr, isDigit )

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = White | Black deriving Show
data Cell = Empty | General Player | Soldier Player | Flag Player deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) White White = True
  (==) Black Black = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Soldier p1) (Soldier p2) = p1 == p2
  (==) (General p1) (General p2) = p1 == p2
  (==) (Flag p1) (Flag p2) = p1 == p2
  (==) _ _ = False


-- ##############################################################################
-- ################## IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                   ###################
-- ################## - 1 Coverage Point                      ###################
-- #############################################################################

-- lies im Forum die Bedingungen
validateFEN :: String -> Bool
validateFEN str = countSlashes str && countBetweenSlashes 0 str 
  where
    -- Listenfunktionale, um '/' herauszufiltern
    countSlashes :: String -> Bool
    countSlashes str = length (filter (== '/') str) == 9
    -- Pattern Match und Guards
    countBetweenSlashes :: Int -> String -> Bool
    countBetweenSlashes n [] = n == 10 || n == 0
    countBetweenSlashes n ('/':xs) 
      | n == 10 || n == 0 = countBetweenSlashes 0 xs
      | otherwise         = False
    countBetweenSlashes n (x:xs)
      | isDigit x   = countBetweenSlashes (n + digitToInt x) xs
      | x `elem` ['w','W','g','b','B','G'] = countBetweenSlashes (n + 1) xs
      | otherwise   = False


-- ##############################################################################
-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 2 Functional Points                   ###################
-- ################## - 1 Coverage Point                      ###################
-- ##############################################################################

-- Es muss nicht geprüft werden, nur bilden
-- für mehr Info über replicate: https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:replicate
buildBoard :: String -> Board
buildBoard fed = evalStr fed (replicate 10 (replicate 10 Empty)) (Pos 'a' 9)
  where
    -- lese jedes Zeichen und setze dementsprechend den Wert in der dazugehörigen Zelle mithilfe von setCell
    evalStr :: String -> Board -> Pos -> Board
    evalStr [] board _ = board
    evalStr ('/':xs) board (Pos _ row) =
        evalStr xs board (Pos 'a' (row - 1))
    evalStr ('W':xs) board (Pos col row) =
        evalStr xs (setCell (Pos col row) (Flag White) board) (Pos (succ col) row)
    evalStr ('w':xs) board (Pos col row) =
        evalStr xs (setCell (Pos col row) (Soldier White) board) (Pos (succ col) row)
    evalStr ('g':xs) board (Pos col row) =
        evalStr xs (setCell (Pos col row) (General White) board) (Pos(succ col) row)
    evalStr ('B':xs) board (Pos col row) =
        evalStr xs (setCell (Pos col row) (Flag Black) board) (Pos (succ col) row)
    evalStr ('b':xs) board (Pos col row) =
        evalStr xs (setCell (Pos col row) (Soldier Black) board) (Pos (succ col) row)
    evalStr ('G':xs) board (Pos col row) =
        evalStr xs (setCell (Pos col row) (General Black) board) (Pos (succ col) row)
    evalStr (x:xs) board (Pos col row)
        | isDigit x = evalStr xs board (Pos (chr (ord col + digitToInt x)) row)
        | otherwise = [[]]
    -- nutze take & drop, um die ungeänderten Zeilen zu übernehmen
    setCell :: Pos -> Cell -> Board -> Board
    setCell (Pos col row) newVal board =
        take (9-row) board ++
        [take colIndex (board !! (9-row)) ++ [newVal] ++ drop (colIndex + 1) (board !! (9-row))] ++
        drop ((9-row) + 1) board
      where
        colIndex = ord col - ord 'a'

