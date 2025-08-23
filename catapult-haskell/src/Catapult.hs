module Catapult where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Board 
import Data.Char

data Move = Move {start :: Pos, target :: Pos}

instance Show Move where
  show (Move (Pos startC startR) (Pos targetC targetR)) = [startC] ++ show startR ++ "-" ++ [targetC] ++ show targetR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1)) (Move (Pos sc2 sr2) (Pos tc2 tr2)) =
    sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2
--------------------------------------------------------------------
-- fast gleiche Logik in den Schnittstellen !!
-- 1) untersuche, ob die Konfiguration gültig ist.
-- 2) finde die Moves. 
-- 3) filtere die gefundene Moves.
-- Ausnahmen:
-- -bei flagMoves müssen nur die erste bzw. die letzte Zeile untersucht werden. 
-- -bei soldierMoves müssen die Bedrohungen gesucht werden, um die richtigen Schlagmoves bzw. Rückzugmoves zu nehmen. 
-- -bei catapultMoves muss die Richtung gefunden werden und zwischen Schuss und Move unterschieden werden. (Update: Schuss ist auch ein Move)
--------------------------------------------------------------------
-- #################################################################################################
-- ################## IMPLEMENT flagMoves :: Board -> Player -> [Move]           ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

data Catapult = N | NE | E | SE | S | SW | W | NW deriving Show

flagMoves :: Board -> Player -> [Move]
flagMoves board player
  | player == White = 
      if not (hasFlag board White) && not (hasFlag board Black) then findMoves (head board) [] 'a' else []
  | otherwise       = 
      if not (hasFlag board Black) && hasFlag board White then findMoves (last board) [] 'a' else []
  where
    hasFlag :: Board -> Player -> Bool
    hasFlag board player
      | player == White = Flag White `elem` head board
      | otherwise       = Flag Black `elem` last board

    findMoves :: [Cell] -> [Move] -> Char -> [Move]
    findMoves [] moves _ = moves
    findMoves (x:xs) moves col
      | col `elem` ['a', 'j'] = findMoves xs moves (succ col) -- Ecken überspringen
      | x == Empty            = findMoves xs (moves ++ if player == White then [Move (Pos col 9) (Pos col 9)] else [Move (Pos col 0) (Pos col 0)]) (succ col)
      | otherwise             = findMoves xs moves (succ col)
    
    
-- #################################################################################################
-- ################## IMPLEMENT generalMoves :: Board -> Player -> Pos -> [Move] ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

generalMoves :: Board -> Player -> Pos -> [Move]
generalMoves board player (Pos col row) 
  | invalidPos = []
  | otherwise  = findMoves (Pos col row)
  where
    rowIdx = 9 - row
    colIdx = ord col - ord 'a'
    invalidPos = 
      rowIdx < 0 || rowIdx >= length board || 
      colIdx < 0 || colIdx >= length (head board) || 
      board !! rowIdx !! colIdx /= General player

    findMoves :: Pos -> [Move]
    findMoves (Pos col row) =
      filter isValidMove
        [ Move (Pos col row) (Pos (pred col) row) 
        , Move (Pos col row) (Pos (succ col) row) 
        , Move (Pos col row) (Pos col (row - 1)) 
        , Move (Pos col row) (Pos col (row + 1)) 
        , Move (Pos col row) (Pos (pred col) (row - 1)) 
        , Move (Pos col row) (Pos (pred col) (row + 1)) 
        , Move (Pos col row) (Pos (succ col) (row - 1)) 
        , Move (Pos col row) (Pos (succ col) (row + 1)) 
        ]

    isValidMove :: Move -> Bool
    isValidMove (Move _ (Pos col row)) =
      let rowIdx = 9 - row  
          colIdx = ord col - ord 'a' 
      in rowIdx >= 0 && rowIdx < length board &&
         colIdx >= 0 && colIdx < length (head board) &&
         board !! rowIdx !! colIdx == Empty

-- #################################################################################################
-- ################## IMPLEMENT soldierMoves :: Board -> Player -> Pos -> [Move] ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################
soldierMoves :: Board -> Player -> Pos -> [Move]
soldierMoves board player (Pos col row) 
  | invalidPos = []
  | otherwise  = findMoves (Pos col row)
  where
    rowIdx = 9 - row
    colIdx = ord col - ord 'a'
    invalidPos = 
      rowIdx < 0 || rowIdx >= length board || 
      colIdx < 0 || colIdx >= length (head board) || 
      board !! rowIdx !! colIdx /= Soldier player
    isFrontRow = if player == White then row - 1 >= 0 else row + 1 <= 9
    isLeftCol = ord (pred col) >= ord 'a'
    isRightCol = ord (succ col) <= ord 'j'
    canCaptureLeft = (isFrontRow && isLeftCol) && checkCaptureLeft (Pos col row)
    canCaptureRight = (isFrontRow && isRightCol) && checkCaptureRight (Pos col row)
    -- canCaptureFront = if isFrontRow then checkCaptureFront (Pos col row) else False     - not needed
    canBeThreatened = checkThreat (Pos col row) isFrontRow isLeftCol isRightCol
    -- Find the possible moves for the soldier
    findMoves :: Pos -> [Move]
    findMoves (Pos col row) = 
      let 
        forwardMove = [Move (Pos col row) (Pos col (if player == White then row - 1 else row + 1))]  -- move one step forward
        sidewaysMoves = [ Move (Pos col row) (Pos (pred col) row),     -- move left
                          Move (Pos col row) (Pos (succ col) row) ]    -- move right
        -- Diagonal captures only allowed if there's an enemy piece
        -- Used list comprehention from https://wiki.haskell.org/List_comprehension
        diagonalCaptureLeft = ([Move (Pos col row) (Pos (pred col) (if player == White then row - 1 else row + 1)) | canCaptureLeft])
        diagonalCaptureRight = ([Move (Pos col row) (Pos (succ col) (if player == White then row - 1 else row + 1)) | canCaptureRight])
        -- Retreat moves if threatened (valid only if spaces are empty)
        retreatMoves =
          if canBeThreatened
          then [Move (Pos col row) (Pos col (if player == White then row + 2 else row - 2)),                 -- retreat vertically
                Move (Pos col row) (Pos (pred (pred col)) (if player == White then row + 2 else row - 2)),   -- retreat diagonal left
                Move (Pos col row) (Pos (succ (succ col)) (if player == White then row + 2 else row - 2)) ]  -- retreat diagonal right
          else []
      in 
        filter isValidMove forwardMove ++ filter isEmpty sidewaysMoves ++ filter isValidMove (diagonalCaptureLeft ++ diagonalCaptureRight) ++ filter isEmpty retreatMoves

    -- Check if the soldier is threatened by an enemy soldier
    checkCaptureLeft :: Pos -> Bool
    checkCaptureLeft (Pos col row) = 
      let frontRow = if player == White then row - 1 else row + 1
          diagonalLeft = 
            board !! (9 - frontRow) !! (ord col - ord 'a' - 1) == Soldier (opponent player) ||
            board !! (9 - frontRow) !! (ord col - ord 'a' - 1) == General (opponent player) || 
            board !! (9 - frontRow) !! (ord col - ord 'a' - 1) == Flag (opponent player)
      in diagonalLeft
    checkCaptureRight :: Pos -> Bool
    checkCaptureRight (Pos cx cy) = 
      let frontRow = if player == White then cy - 1 else cy + 1
          diagonalRight = 
            board !! (9 - frontRow) !! (ord cx - ord 'a' + 1) == Soldier (opponent player) ||
            board !! (9 - frontRow) !! (ord cx - ord 'a' + 1) == General (opponent player) ||
            board !! (9 - frontRow) !! (ord cx - ord 'a' + 1) == Flag (opponent player)
      in diagonalRight
{-     checkCaptureFront :: Pos -> Bool
    checkCaptureFront (Pos cx cy) = 
      let frontRow = if player == White then cy - 1 else cy + 1
          front = 
            board !! (9 - frontRow) !! (ord cx - ord 'a') == Soldier (opponent player) ||
            board !! (9 - frontRow) !! (ord cx - ord 'a') == General (opponent player) ||
            board !! (9 - frontRow) !! (ord cx - ord 'a') == Flag (opponent player)
      in front  -}
    checkThreat :: Pos -> Bool -> Bool -> Bool -> Bool 
    checkThreat (Pos col row) isFrontRow isLeftCol isRightCol =
      let frontRow = if player == White then row - 1 else row + 1
          threat = 
            isFrontRow && (
            (board !! (9 - frontRow) !! (ord col - ord 'a') == Soldier (opponent player)) ||
            (isLeftCol && board !! (9 - frontRow) !! (ord col - ord 'a' - 1) == Soldier (opponent player)) ||
            (isRightCol && board !! (9 - frontRow) !! (ord col - ord 'a' + 1) == Soldier (opponent player))
            )
      in threat 
        
    -- Get the opponent player
    opponent :: Player -> Player
    opponent White = Black
    opponent Black = White

    isEmpty :: Move -> Bool
    isEmpty (Move _ (Pos col row)) =
      let rowIdx = 9 - row  
          colIdx = ord col - ord 'a' 
      in rowIdx >= 0 && rowIdx < length board &&
         colIdx >= 0 && colIdx < length (head board) &&
         board !! rowIdx !! colIdx == Empty

    isValidMove :: Move -> Bool
    isValidMove (Move _ (Pos col row)) =
      let rowIdx = 9 - row  
          colIdx = ord col - ord 'a' 
      in rowIdx >= 0 && rowIdx < length board &&
         colIdx >= 0 && colIdx < length (head board) &&
         (board !! rowIdx !! colIdx /= Soldier player &&
         board !! rowIdx !! colIdx /= General player &&
         board !! rowIdx !! colIdx /= Flag player)

-- #################################################################################################
-- ################## IMPLEMENT catapultMoves :: Board -> Player -> Pos -> [Move]  ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

catapultMoves :: Board -> Player -> Pos -> [Move]
catapultMoves board player (Pos col row)
  | board!!(9-row)!!(ord col - ord 'a') /= Soldier player = []
  | null catapults                                        = []
  | otherwise                                             = findMoves catapults []
  where
    eqCatapult :: Catapult -> Catapult -> Bool
    eqCatapult N N   = True
    eqCatapult S S   = True
    eqCatapult E E   = True
    eqCatapult W W   = True
    eqCatapult NE NE = True
    eqCatapult NW NW = True
    eqCatapult SE SE = True
    eqCatapult SW SW = True
    eqCatapult _ _   = False
    checkCatapult :: Board -> Pos -> [Catapult]
    checkCatapult board (Pos col row) =
      let
        rowIdx = 9 - row  -- Assuming row is indexed from top (1) to bottom (9)
        colIdx = ord col - ord 'a'  -- Column index from 'a'
        catapults =
          [E | colIdx + 1 < length (board !! rowIdx) , colIdx + 2 < length (board !! rowIdx) ,
                    board !! rowIdx !! colIdx == board !! rowIdx !! (colIdx + 1) ,
                    board !! rowIdx !! colIdx == board !! rowIdx !! (colIdx + 2)]
          ++
          [W | colIdx - 1 >= 0 , colIdx - 2 >= 0 ,
                    board !! rowIdx !! colIdx == board !! rowIdx !! (colIdx - 1) ,
                    board !! rowIdx !! colIdx == board !! rowIdx !! (colIdx - 2)]
          ++
          [N | rowIdx - 1 >= 0 , rowIdx - 2 >= 0 ,
                    board !! rowIdx !! colIdx == board !! (rowIdx - 1) !! colIdx ,
                    board !! rowIdx !! colIdx == board !! (rowIdx - 2) !! colIdx]
          ++
          [S | rowIdx + 1 < length board , rowIdx + 2 < length board ,
                    board !! rowIdx !! colIdx == board !! (rowIdx + 1) !! colIdx ,
                    board !! rowIdx !! colIdx == board !! (rowIdx + 2) !! colIdx]           
          ++
          [NE | rowIdx - 1 >= 0 , colIdx + 1 < length (board !! rowIdx) ,
                    rowIdx - 2 >= 0 , colIdx + 2 < length (board !! rowIdx) ,
                    board !! rowIdx !! colIdx == board !! (rowIdx - 1) !! (colIdx + 1) ,
                    board !! rowIdx !! colIdx == board !! (rowIdx - 2) !! (colIdx + 2)]
          ++
          [NW | rowIdx - 1 >= 0 && colIdx - 1 >= 0 &&
                    rowIdx - 2 >= 0 && colIdx - 2 >= 0 &&
                    board !! rowIdx !! colIdx == board !! (rowIdx - 1) !! (colIdx - 1) &&
                    board !! rowIdx !! colIdx == board !! (rowIdx - 2) !! (colIdx - 2)]
          ++
          [SE | rowIdx + 1 < length board , colIdx + 1 < length (board !! rowIdx) ,
                    rowIdx + 2 < length board , colIdx + 2 < length (board !! rowIdx) ,
                    board !! rowIdx !! colIdx == board !! (rowIdx + 1) !! (colIdx + 1) ,
                    board !! rowIdx !! colIdx == board !! (rowIdx + 2) !! (colIdx + 2)]
          ++
          [SW | rowIdx + 1 < length board , colIdx - 1 >= 0 ,
                    rowIdx + 2 < length board , colIdx - 2 >= 0 ,
                    board !! rowIdx !! colIdx == board !! (rowIdx + 1) !! (colIdx - 1) ,
                    board !! rowIdx !! colIdx == board !! (rowIdx + 2) !! (colIdx - 2)]
      in
        catapults

    catapults = checkCatapult board (Pos col row)
    findMoves :: [Catapult] -> [Move] -> [Move]
    findMoves [] moves  = moves
    findMoves (x:xs) moves 
      | eqCatapult x N    = findMoves xs (moves ++ filter isValidMove [Move (Pos col row) (Pos col (row+3))] ++ (if generalCommand N (Pos col row) then filter isValidShot [Move (Pos col row) (Pos col (row+4)), Move (Pos col row) (Pos col (row+5))] else []))
      | eqCatapult x S    = findMoves xs (moves ++ filter isValidMove [Move (Pos col row) (Pos col (row-3))] ++ (if generalCommand S (Pos col row) then filter isValidShot [Move (Pos col row) (Pos col (row-4)), Move (Pos col row) (Pos col (row-5))] else []))
      | eqCatapult x E    = findMoves xs (moves ++ filter isValidMove [Move (Pos col row) (Pos (chr(ord col+3)) row)] ++ (if generalCommand E (Pos col row) then filter isValidShot [Move (Pos col row) (Pos (chr(ord col+4)) row), Move (Pos col row) (Pos (chr(ord col+5)) row)] else []))
      | eqCatapult x W    = findMoves xs (moves ++ filter isValidMove [Move (Pos col row) (Pos (chr(ord col-3)) row)] ++ (if generalCommand W (Pos col row) then filter isValidShot [Move (Pos col row) (Pos (chr(ord col-4)) row), Move (Pos col row) (Pos (chr(ord col-5)) row)] else []))
      | eqCatapult x NE   = findMoves xs (moves ++ filter isValidMove [Move (Pos col row) (Pos (chr(ord col+3)) (row+3))] ++ (if generalCommand NE (Pos col row) then filter isValidShot [Move (Pos col row) (Pos (chr(ord col+4)) (row+4)), Move (Pos col row) (Pos (chr(ord col+5)) (row+5))] else []))
      | eqCatapult x NW   = findMoves xs (moves ++ filter isValidMove [Move (Pos col row) (Pos (chr(ord col-3)) (row+3))] ++ (if generalCommand NW (Pos col row) then filter isValidShot [Move (Pos col row) (Pos (chr(ord col-4)) (row+4)), Move (Pos col row) (Pos (chr(ord col-5)) (row+5))] else []))
      | eqCatapult x SE   = findMoves xs (moves ++ filter isValidMove [Move (Pos col row) (Pos (chr(ord col+3)) (row-3))] ++ (if generalCommand SE (Pos col row) then filter isValidShot [Move (Pos col row) (Pos (chr(ord col+4)) (row-4)), Move (Pos col row) (Pos (chr(ord col+5)) (row-5))] else []))
      | eqCatapult x SW   = findMoves xs (moves ++ filter isValidMove [Move (Pos col row) (Pos (chr(ord col-3)) (row-3))] ++ (if generalCommand SW (Pos col row) then filter isValidShot [Move (Pos col row) (Pos (chr(ord col-4)) (row-4)), Move (Pos col row) (Pos (chr(ord col-5)) (row-5))] else []))
      where
        isValidMove :: Move -> Bool
        isValidMove (Move _ (Pos col row)) =
          let rowIdx = 9 - row  
              colIdx = ord col - ord 'a' 
          in rowIdx >= 0 && rowIdx < length board &&
            colIdx >= 0 && colIdx < length (head board) &&
            board !! rowIdx !! colIdx == Empty

        opponent :: Player -> Player
        opponent White = Black
        opponent Black = White

        isValidShot :: Move -> Bool 
        isValidShot (Move (Pos ocol orow) (Pos col row)) =
          let rowIdx = 9 - row  
              colIdx = ord col - ord 'a' 
          in rowIdx >= 0 && rowIdx < length board &&
            colIdx >= 0 && colIdx < length (head board) &&
            (board !! rowIdx !! colIdx == Soldier (opponent player) || 
            board !! rowIdx !! colIdx == General (opponent player) || 
            board !! rowIdx !! colIdx == Flag (opponent player))

        generalCommand :: Catapult -> Pos -> Bool 
        generalCommand dir (Pos col row) 
          | eqCatapult dir N = 
            Pos col row `elem` generalTargets board || 
            Pos col (row+1) `elem` generalTargets board || 
            Pos col (row+2) `elem` generalTargets board 
          | eqCatapult dir S =
            Pos col row `elem` generalTargets board || 
            Pos col (row-1) `elem` generalTargets board || 
            Pos col (row-2) `elem` generalTargets board 
          | eqCatapult dir E = 
            Pos col row `elem` generalTargets board || 
            Pos (succ col) row `elem` generalTargets board || 
            Pos (succ (succ col)) row `elem` generalTargets board 
          | eqCatapult dir W = 
            Pos col row `elem` generalTargets board || 
            Pos (pred col) row `elem` generalTargets board || 
            Pos (pred (pred col)) row `elem` generalTargets board 
          | eqCatapult dir NE = 
            Pos col row `elem` generalTargets board || 
            Pos (succ col) (row+1) `elem` generalTargets board || 
            Pos (succ (succ col)) (row+2) `elem` generalTargets board 
          | eqCatapult dir NW = 
            Pos col row `elem` generalTargets board || 
            Pos (pred col) (row+1) `elem` generalTargets board || 
            Pos (pred (pred col)) (row+2) `elem` generalTargets board 
          | eqCatapult dir SE = 
            Pos col row `elem` generalTargets board || 
            Pos (succ col) (row-1) `elem` generalTargets board || 
            Pos (succ (succ col)) (row-2) `elem` generalTargets board 
          | eqCatapult dir SW = 
            Pos col row `elem` generalTargets board || 
            Pos (pred col) (row-1) `elem` generalTargets board || 
            Pos (pred (pred col)) (row-2) `elem` generalTargets board 
          where
            generalTargets :: Board -> [Pos]
            generalTargets board = 
              let generalpos = findGeneral board (Pos 'a' 9) 
              in findGeneralTargets generalpos
                where
                  findGeneral :: Board -> Pos -> Pos
                  findGeneral [] pos = pos
                  findGeneral ([]:r) (Pos col row) = findGeneral r (Pos 'a' (row-1))
                  findGeneral ((x:xs):r) (Pos col row)
                    | x == General player = Pos col row
                    | otherwise           = findGeneral (xs:r) (Pos (succ col) row)
                  findGeneralTargets :: Pos -> [Pos]
                  findGeneralTargets (Pos col row) = 
                    [ Pos (pred col) row 
                    , Pos (succ col) row 
                    , Pos col (row - 1) 
                    , Pos col (row + 1) 
                    , Pos (pred col) (row - 1) 
                    , Pos (pred col) (row + 1) 
                    , Pos (succ col) (row - 1) 
                    , Pos (succ col) (row + 1) 
                    ]
---------------------------------------------------------------------
-- die Hilfsfunktionen wiederholen sich zum Teil
-- hier kann listMoves genutzt werden und daher zuerst implementiert
---------------------------------------------------------------------
-- #################################################################################################
-- ################## IMPLEMENT playerWon :: Board -> Maybe Player               ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

playerWon :: Board -> Player -> Bool
playerWon board player 
  | not (hasFlag board (opponent player)) = True
  | null (listMoves board (opponent player))   = True
  | not (hasGeneral board (opponent player))   = True
  | otherwise                                  = False
  where
    opponent :: Player -> Player
    opponent White = Black
    opponent Black = White
    hasFlag :: Board -> Player -> Bool
    hasFlag board player 
      | player == White = Flag White `elem` head board 
      | otherwise       = Flag Black `elem` last board
    hasGeneral :: Board -> Player -> Bool 
    hasGeneral [] _ = False
    hasGeneral (x:xs) player 
      | General player `elem` x = True
      | otherwise               = hasGeneral xs player
      
--------------------------------------------------------------------------------
-- Iteration über jedes Feld. Interessant sind nur Soldier und General
-- bei jeder Iteration die Position aktualisieren
--------------------------------------------------------------------------------
-- #################################################################################################
-- ################## IMPLEMENT listMoves :: Board -> Player -> [Move]           ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

listMoves :: Board -> Player -> [Move]
listMoves board player
  | Flag player `notElem` (if player == White then head board else last board) = flagMoves board player 
  | otherwise                                                                  = findMoves board player (Pos 'a' 9) []
  where 
    findMoves :: Board -> Player -> Pos -> [Move] -> [Move]
    findMoves [] _ _ moves = moves
    findMoves ([]:r) player (Pos col row) moves = findMoves r player (Pos 'a' (row-1)) moves
    findMoves ((x:xs):r) player (Pos col row) moves
      | x == Soldier player = findMoves (xs:r) player (Pos (succ col) row) (moves ++ soldierMoves board player (Pos col row) ++ catapultMoves board player (Pos col row))
      | x == General player = findMoves (xs:r) player (Pos (succ col) row) (moves ++ generalMoves board player (Pos col row))
      | otherwise           = findMoves (xs:r) player (Pos (succ col) row) moves
