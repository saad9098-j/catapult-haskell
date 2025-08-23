-- #############################################################################
-- ###### GRADING TESTS                                               ##########
-- #############################################################################

import Test.Hspec

import Board
     (validateFEN,
      buildBoard,
      Player(White, Black),
      Cell(Empty, Flag, Soldier, General),
      Pos(Pos, col, row))
import Catapult



sampleBoard = [
                  [Empty,Empty,Empty,Empty,Flag White,Empty,Empty,Empty,Empty,Empty],
                  [Empty,Soldier White,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White],
                  [Empty,Soldier White,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White],
                  [Empty,Soldier White,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White],
                  [Empty,Empty,Empty,Empty,Empty,General White,Empty,Empty,Empty,Empty],
                  [Empty,Empty,Empty,Empty,General Black,Empty,Empty,Empty,Empty,Empty],
                  [Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty],
                  [Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty],
                  [Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty],
                  [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Flag Black,Empty,Empty]]


sampleBoard6 = [ [Empty,Empty,Empty,Empty,Flag White,Empty,Empty,Empty,Empty,Empty],
                [Empty,Soldier White,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White],
                [Empty,Soldier White,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White],
                [Empty,Empty,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White,Empty,Soldier White],
                [Empty,Empty,Empty,Empty,Empty,General White,Empty,Empty,Empty,Empty],
                [Empty,Soldier White,Empty,Empty,General Black,Empty,Empty,Empty,Empty,Empty],
                [Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty],
                [Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty],
                [Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty,Soldier Black,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Flag Black,Empty,Empty]]

sampleBoard1 = [[Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White],
                [Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White],
                [Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White],
                [Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White],
                [Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White],
                [Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White],
                [Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White],
                [Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White],
                [Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White],
                [Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White,Soldier White]]

sampleBoard2 =  [
                  [Flag White,Flag White,Flag White,Flag White,Flag White,Flag White,Flag White,Flag White,Flag White,Flag White],
                  [General White,General White,General White,General White,General White,General White,General White,General White,General White,General White],
                  [Flag Black,Flag Black,Flag Black,Flag Black,Flag Black,Flag Black,Flag Black,Flag Black,Flag Black,Flag Black],
                  [General Black,General Black,General Black,General Black,General Black,General Black,General Black,General Black,General Black,General Black],
                  [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                  [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                  [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                  [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                  [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                  [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]

main :: IO ()
main = hspec $ do
    testEqPos
    testPosFields
    testMovefields
    testShowMove
    testvalidateFEN
    testValidateBuildBoard
    testFlagMoves
    testGeneralMoves
    testSoldierMoves
    testCatapultMoves
    testPlayerWon
    testListMoves

-- remaining Tests
testEqPos :: Spec
testEqPos = describe "Pos Eq instance" $ do
  it "returns True for equal positions" $ do
    (Pos 'a' 1 == Pos 'a' 1) `shouldBe` True
  it "returns False for different positions" $ do
    (Pos 'a' 1 == Pos 'b' 3) `shouldBe` False

testPosFields :: Spec
testPosFields = describe "Pos data type" $ do
  it "should correctly store column and row" $ do
    let pos = Pos 'a' 1
    col pos `shouldBe` 'a'
    row pos `shouldBe` 1

testMovefields :: Spec
testMovefields = describe "Move data type" $ do
  it "should correctly store startpos and targetpos" $ do
    let move = Move (Pos 'a' 0) (Pos 'j' 9)
    start move `shouldBe` Pos 'a' 0
    target move `shouldBe` Pos 'j' 9

testShowMove :: Spec
testShowMove = describe "Show move" $ do
  it "should show move as intended" $ do
    let move = Move (Pos 'a' 0) (Pos 'j' 9)
    show move `shouldBe` "a0-j9"

-- Tests for validateFEN
testvalidateFEN :: Spec
testvalidateFEN = describe "coverage of validateFEN: " $ do
        it "less columns is not valid" $ do
            validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` False
        it "zero columns is valid" $ do
            validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/" `shouldBe` True
        it "less rows is not valid" $ do
            validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g44G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` False
        it "invalid char" $ do
            validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5L4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` False

-- Tests for buildBoard
testValidateBuildBoard :: Spec
testValidateBuildBoard = describe "coverage of buildBoard: " $ do
        it "empty board" $ do
            buildBoard "10/10/10/10/10/10/10/10/10/10" `shouldBe` replicate 10 (replicate 10 Empty)
        it "example board" $ do
            buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` sampleBoard
        it "all white soldiers" $ do
            buildBoard "wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww" `shouldBe` sampleBoard1
        it "first row all white Flags, second white generals, third black flags, fourth black Generals" $ do
            buildBoard "WWWWWWWWWW/gggggggggg/BBBBBBBBBB/GGGGGGGGGG/10/10/10/10/10/10" `shouldBe` sampleBoard2
        it "invalid char" $ do
            buildBoard "10/10/9v/10/10/10/10/10/10/10" `shouldBe` [[]]

-- Tests for flagMoves
testFlagMoves:: Spec
testFlagMoves = describe "coverage of flagMoves: " $ do
        it "empty board is valid start somehow" $ do
            flagMoves (buildBoard "10/10/10/10/10/10/10/10/10/10") White /= [] `shouldBe` True
        it "valid board for white" $ do
            let moves = [
                          Move (Pos 'b' 9) (Pos 'b' 9), 
                          Move (Pos 'c' 9) (Pos 'c' 9), 
                          Move (Pos 'd' 9) (Pos 'd' 9),
                          Move (Pos 'e' 9) (Pos 'e' 9), 
                          Move (Pos 'f' 9) (Pos 'f' 9), 
                          Move (Pos 'g' 9) (Pos 'g' 9), 
                          Move (Pos 'h' 9) (Pos 'h' 9), 
                          Move (Pos 'i' 9) (Pos 'i' 9)
                        ]
            flagMoves (buildBoard "10/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/10") White `shouldBe` moves
        it "board for white's turn but black town already placed" $ do
            flagMoves (buildBoard "10/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2") White `shouldBe` []
        it "valid board for black" $ do
            let moves = [
                          Move (Pos 'b' 0) (Pos 'b' 0), 
                          Move (Pos 'c' 0) (Pos 'c' 0), 
                          Move (Pos 'd' 0) (Pos 'd' 0), 
                          Move (Pos 'e' 0) (Pos 'e' 0), 
                          Move (Pos 'f' 0) (Pos 'f' 0), 
                          Move (Pos 'g' 0) (Pos 'g' 0), 
                          Move (Pos 'h' 0) (Pos 'h' 0), 
                          Move (Pos 'i' 0) (Pos 'i' 0)
                        ]
            flagMoves (buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/10") Black `shouldBe` moves
        it "black's flag already placed" $ do
            flagMoves (buildBoard "10/10/9v/10/10/10/10/10/10/7B2") Black `shouldBe` []
        it "black's flag not placed but also not white's" $ do
            flagMoves sampleBoard1 Black `shouldBe` []
        it "white flag but the row is all soldier white" $ do
            flagMoves sampleBoard1 White `shouldBe` []

-- Tests for generalMoves
testGeneralMoves:: Spec
testGeneralMoves = describe "coverage of generalMoves: " $ do
        it "white general moves" $ do
            let moves = [
                          Move (Pos 'f' 5) (Pos 'e' 5), 
                          Move (Pos 'f' 5) (Pos 'g' 5), 
                          Move (Pos 'f' 5) (Pos 'f' 4), 
                          Move (Pos 'f' 5) (Pos 'e' 6), 
                          Move (Pos 'f' 5) (Pos 'g' 4), 
                          Move (Pos 'f' 5) (Pos 'g' 6)
                        ]
            generalMoves sampleBoard White (Pos 'f' 5) `shouldBe` moves
        it "black general moves" $ do
            let moves = [
                          Move (Pos 'e' 4) (Pos 'd' 4), 
                          Move (Pos 'e' 4) (Pos 'f' 4), 
                          Move (Pos 'e' 4) (Pos 'e' 5), 
                          Move (Pos 'e' 4) (Pos 'd' 3), 
                          Move (Pos 'e' 4) (Pos 'd' 5), 
                          Move (Pos 'e' 4) (Pos 'f' 3)
                        ]
            generalMoves sampleBoard Black (Pos 'e' 4) `shouldBe` moves
        it "not general at the position" $ do
            generalMoves sampleBoard1 White (Pos 'b' 7) `shouldBe` []

-- Tests for soldierMoves
testSoldierMoves:: Spec
testSoldierMoves = describe "coverage of soldierMoves: " $ do
        it "retreat moves are blocked" $ do
            let moves = [
                          Move (Pos 'b' 4) (Pos 'b' 3), 
                          Move (Pos 'b' 4) (Pos 'a' 4), 
                          Move (Pos 'b' 4) (Pos 'c' 4), 
                          Move (Pos 'b' 4) (Pos 'a' 3), 
                          Move (Pos 'b' 4) (Pos 'c' 3),
                          Move (Pos 'b' 4) (Pos 'b' 6)
                        ]
            soldierMoves sampleBoard6 White (Pos 'b' 4) `shouldBe` moves
        it "not soldier of the player at the positions" $ do
            soldierMoves sampleBoard1 Black (Pos 'c' 6) `shouldBe` []
        it "black soldier moves" $ do
            let moves = [
                          Move (Pos 'a' 3) (Pos 'a' 4),
                          Move (Pos 'a' 3) (Pos 'b' 3)
                        ]
            soldierMoves sampleBoard Black (Pos 'a' 3) `shouldBe` moves
        it "black captures left and right and retreats" $ do
            let moves = [
                          Move (Pos 'd' 5) (Pos 'd' 6),
                          Move (Pos 'd' 5) (Pos 'c' 5),
                          Move (Pos 'd' 5) (Pos 'e' 5),
                          Move (Pos 'd' 5) (Pos 'c' 6),
                          Move (Pos 'd' 5) (Pos 'e' 6),
                          Move (Pos 'd' 5) (Pos 'd' 3),
                          Move (Pos 'd' 5) (Pos 'b' 3),
                          Move (Pos 'd' 5) (Pos 'f' 3)
                        ]
            soldierMoves (buildBoard "///2w1w5/3b6/////") Black (Pos 'd' 5) `shouldBe` moves
        it "white is not threatened from right" $ do
            let moves = [
                          Move (Pos 'b' 8) (Pos 'b' 7),
                          Move (Pos 'b' 8) (Pos 'a' 8),
                          Move (Pos 'b' 8) (Pos 'c' 8),
                          Move (Pos 'b' 8) (Pos 'a' 7)
                        ]
            soldierMoves (buildBoard "/1w8/bb8///////") White (Pos 'b' 8) `shouldBe` moves
        it "right threat for white is out of borders" $ do
            let moves = [
                          Move (Pos 'j' 8) (Pos 'j' 7), 
                          Move (Pos 'j' 8) (Pos 'i' 8)
                        ]
            soldierMoves (buildBoard "/9w/9b///////") White (Pos 'j' 8) `shouldBe` moves
        it "additional test for coverage" $ do
            let moves1 = [
                          Move (Pos 'c' 8) (Pos 'c' 9),
                          Move (Pos 'c' 8) (Pos 'b' 8),
                          Move (Pos 'c' 8) (Pos 'b' 9),
                          Move (Pos 'c' 8) (Pos 'd' 9),
                          Move (Pos 'c' 8) (Pos 'c' 6),
                          Move (Pos 'c' 8) (Pos 'a' 6),
                          Move (Pos 'c' 8) (Pos 'e' 6)
                         ]
            let moves2 = [
                          Move (Pos 'd' 8) (Pos 'd' 9),
                          Move (Pos 'd' 8) (Pos 'e' 8),
                          Move (Pos 'd' 8) (Pos 'c' 9),
                          Move (Pos 'd' 8) (Pos 'e' 9),
                          Move (Pos 'd' 8) (Pos 'd' 6),
                          Move (Pos 'd' 8) (Pos 'b' 6),
                          Move (Pos 'd' 8) (Pos 'f' 6)
                         ]
            let board = buildBoard "1WwgW5/11bb6///////8bw/7bbb" 
            soldierMoves board Black (Pos 'c' 8) `shouldBe` moves1
            soldierMoves board Black (Pos 'd' 8) `shouldBe` moves2
            soldierMoves board Black (Pos 'i' 0) `shouldBe` [Move (Pos 'i' 0) (Pos 'j' 1)]

-- Tests for catapultMoves
testCatapultMoves :: Spec
testCatapultMoves = describe "coverage of catapultMoves: " $ do
        it "empty at the position" $ do
            catapultMoves sampleBoard1 White (Pos 'c' 7) `shouldBe` []
        it "not a soldier of player at position" $ do
            catapultMoves sampleBoard2 Black (Pos 'b' 8) `shouldBe` []
        it "middle soldier, so no catapults" $ do
            catapultMoves sampleBoard2 White (Pos 'b' 7) `shouldBe` []
        it "valid catapult south" $ do
            let moves = [Move (Pos 'b' 8) (Pos 'b' 5), Move (Pos 'b' 8) (Pos 'b' 4), Move (Pos 'b' 8) (Pos 'b' 3)]
            catapultMoves (buildBoard "28/1wg7/1w8/1w8//1b8/1b8///") White (Pos 'b' 8) `shouldBe` moves
            catapultMoves (buildBoard "28/1w8/1wg7/1w8//1b8/1b8///") White (Pos 'b' 8) `shouldBe` moves
            catapultMoves (buildBoard "28/1w8/1w17/1w8/g9/1b8/1b8///") White (Pos 'b' 8) `shouldBe` moves
        it "valid catapult north" $ do
            let moves = [Move (Pos 'b' 1) (Pos 'b' 4), Move (Pos 'b' 1) (Pos 'b' 5), Move (Pos 'b' 1) (Pos 'b' 6)]
            catapultMoves (buildBoard "///1w8/1w8//1b8/1b8/1bG7/") Black (Pos 'b' 1) `shouldBe` moves
            catapultMoves (buildBoard "///1w8/1w8//1bG7/1b8/1b17/") Black (Pos 'b' 1) `shouldBe` moves
            catapultMoves (buildBoard "///1w8/1w8/G9/1b8/1b8/1bG7/") Black (Pos 'b' 1) `shouldBe` moves        
        it "valid catapult west" $ do
            let moves = [Move (Pos 'h' 8) (Pos 'e' 8), Move (Pos 'h' 8) (Pos 'd' 8), Move (Pos 'h' 8) (Pos 'c' 8)]
            catapultMoves (buildBoard "/2bb1wwwg1////////") White (Pos 'h' 8) `shouldBe` moves
            catapultMoves (buildBoard "5g4/2bb1www2////////") White (Pos 'h' 8) `shouldBe` moves
            catapultMoves (buildBoard "4g5/2bb1www2////////") White (Pos 'h' 8) `shouldBe` moves
        it "valid catapult east" $ do
            let moves = [Move (Pos 'c' 8) (Pos 'f' 8), Move (Pos 'c' 8) (Pos 'g' 8), Move (Pos 'c' 8) (Pos 'h' 8)]
            catapultMoves (buildBoard "/1gwww1bb2////////") White (Pos 'c' 8) `shouldBe` moves
            catapultMoves (buildBoard "4g5/2www1bb2////////") White (Pos 'c' 8) `shouldBe` moves
            catapultMoves (buildBoard "5g4/2www1bb2////////") White (Pos 'c' 8) `shouldBe` moves
        it "valid catapult southeast" $ do
            let moves = [Move (Pos 'a' 8) (Pos 'd' 5), Move (Pos 'a' 8) (Pos 'e' 4), Move (Pos 'a' 8) (Pos 'f' 3)]
            catapultMoves (buildBoard "/wg8/1w8/2w7//4b5/5b4///") White (Pos 'a' 8) `shouldBe` moves
            catapultMoves (buildBoard "/w9/1w8/1gw7//4b5/5b4///") White (Pos 'a' 8) `shouldBe` moves
            catapultMoves (buildBoard "/w9/1w8/2w7/2g7/4b5/5b4///") White (Pos 'a' 8) `shouldBe` moves
        it "valid catapult southwest" $ do
            let moves = [Move (Pos 'j' 8) (Pos 'g' 5), Move (Pos 'j' 8) (Pos 'f' 4), Move (Pos 'j' 8) (Pos 'e' 3)]
            catapultMoves (buildBoard "9g/9w/7gw1/7w2//5b4/4b5///") White (Pos 'j' 8) `shouldBe` moves
            catapultMoves (buildBoard "/9w/7gw1/7w2//5b4/4b5///") White (Pos 'j' 8) `shouldBe` moves
            catapultMoves (buildBoard "/9w/8w1/7w2/7g2/5b4/4b5///") White (Pos 'j' 8) `shouldBe` moves
        it "valid catapult northeast" $ do
            let moves = [Move (Pos 'a' 1) (Pos 'd' 4), Move (Pos 'a' 1) (Pos 'e' 5), Move (Pos 'a' 1) (Pos 'f' 6)]
            catapultMoves (buildBoard "///5W4/4w5//2b7/1b8/b9/G9") Black (Pos 'a' 1) `shouldBe` moves
            catapultMoves (buildBoard "///5W4/4w5//2b7/1bG7/b9/") Black (Pos 'a' 1) `shouldBe` moves
            catapultMoves (buildBoard "///5W4/4w5/2G7/2b7/1bG7/b9/") Black (Pos 'a' 1) `shouldBe` moves
        it "valid catapult northwest" $ do
            let moves = [Move (Pos 'j' 1) (Pos 'g' 4), Move (Pos 'j' 1) (Pos 'f' 5), Move (Pos 'j' 1) (Pos 'e' 6)]
            catapultMoves (buildBoard "///4g5/5w4//7b2/8b1/9b/9G") Black (Pos 'j' 1) `shouldBe` moves
            catapultMoves (buildBoard "///4g5/5w4//6Gb2/8b1/9b/") Black (Pos 'j' 1) `shouldBe` moves
            catapultMoves (buildBoard "///4g5/5w4//7b1G/8b1/9b/") Black (Pos 'j' 1) `shouldBe` moves
        it "catapult in all directions but no shots" $ do
            let moves = [
                          Move (Pos 'e' 6) (Pos 'h' 6),
                          Move (Pos 'e' 6) (Pos 'b' 6),
                          Move (Pos 'e' 6) (Pos 'e' 9),
                          Move (Pos 'e' 6) (Pos 'e' 3),
                          Move (Pos 'e' 6) (Pos 'h' 9),
                          Move (Pos 'e' 6) (Pos 'b' 9),
                          Move (Pos 'e' 6) (Pos 'h' 3),
                          Move (Pos 'e' 6) (Pos 'b' 3)
                        ]
            catapultMoves (buildBoard "/2b1b1b3/3bbb4/2bbbbb3/3bbb4/2b1b1b3////") Black (Pos 'e' 6) `shouldBe` moves
-- Tests for playerWon
testPlayerWon :: Spec
testPlayerWon = describe "coverage of playerWon: " $ do
        it "black has no flag, white won" $ do
            playerWon sampleBoard1 White `shouldBe` True
        it "white has no flag, black won" $ do
            playerWon sampleBoard1 Black `shouldBe` True
        it "black has flag but no moves, white won" $ do
            playerWon (buildBoard "/////////7B2") White `shouldBe` True
        it "white has flag but no moves, black won" $ do
            playerWon (buildBoard "7W2/////////") Black `shouldBe` True
        it "black has flag and moves but no general, white won" $ do
            playerWon (buildBoard "//////1b8///7B2") White `shouldBe` True
        it "game is not finished" $ do
            playerWon sampleBoard White `shouldBe` False
-- Tests for listMoves
testListMoves :: Spec
testListMoves = describe "coverage of listMoves: " $ do
        it "empty board with only white Flag" $ do
            listMoves (buildBoard "5W4/////////") White `shouldBe` []
        it "empty board with only black Flag" $ do
            listMoves (buildBoard "/////////5B4") White `shouldBe` []
        it "board with catapult and general and flag" $ do
            let moves = [
                          Move (Pos 'b' 8) (Pos 'a' 8),
                          Move (Pos 'b' 8) (Pos 'c' 8),
                          Move (Pos 'b' 8) (Pos 'b' 5),
                          Move (Pos 'a' 7) (Pos 'a' 6),
                          Move (Pos 'a' 7) (Pos 'a' 8),
                          Move (Pos 'b' 7) (Pos 'c' 7),
                          Move (Pos 'b' 6) (Pos 'b' 5),
                          Move (Pos 'b' 6) (Pos 'a' 6),
                          Move (Pos 'b' 6) (Pos 'c' 6),
                          Move (Pos 'b' 6) (Pos 'b' 9),
                          Move (Pos 'f' 6) (Pos 'f' 5),
                          Move (Pos 'f' 6) (Pos 'e' 6),
                          Move (Pos 'f' 6) (Pos 'g' 6)
                        ]
            listMoves (buildBoard "5W4/1w8/gw8/1w3w4//////") White `shouldBe` moves
        it "board with action at Pos 'a' 9" $ do
            let board = buildBoard "wW9/////////"
            listMoves board White `shouldNotBe` []
