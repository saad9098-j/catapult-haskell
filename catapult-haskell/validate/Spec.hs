-- #############################################################################
-- ###### VALIDATION TESTS                                            ##########
-- ###### (DO NOT CHANGE ANYTHING)                                    ##########
-- ###### Note: execute tests using "stack test catapult:validate"      ##########
-- #############################################################################

import Test.Hspec

import Board
     (validateFEN,
      buildBoard,
      Player(White, Black),
      Cell(Empty, Flag, Soldier, General),
      Pos(Pos))

import Catapult (playerWon, flagMoves, soldierMoves, catapultMoves, listMoves)

sampleBoard = [
                [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
                [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
                [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
                [Empty          , Empty          , Empty          , Empty          , Empty          , (General White), Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , (General Black), Empty          , Empty          , Empty          , Empty          , Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Flag Black)   , Empty          , Empty          ]]

noMoveBoard = [
                [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Flag Black)   , Empty          , Empty          ]]

main :: IO ()
main = hspec $ do
    testValidateFEN
    testValidateBuildBoard
    testFlagMoves
    testGeneralMoves
    testSoldierMoves
    testCatapultMoves
    testPlayerWon
    testListMoves

testValidateFEN :: Spec
testValidateFEN = describe "IF Validate-Module-Board: validateFEN ..." $ do
        it "IF empty string is not valid" $ do
            validateFEN "" `shouldBe` (False :: Bool)

testValidateBuildBoard :: Spec
testValidateBuildBoard = describe "IF Validate-Module-Board: buildBoard ..." $ do
        it "test example board" $ do
            buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` sampleBoard

testFlagMoves:: Spec
testFlagMoves = describe "IF Validate-Module-Catapult: flagMoves ..." $ do
        it "town already placed" $ do
            flagMoves sampleBoard White `shouldBe` []

testGeneralMoves:: Spec
testGeneralMoves = describe "IF Validate-Module-Catapult: generalMoves ..." $ do
        it "wrong position" $ do
            soldierMoves sampleBoard White (Pos 'a' 9) `shouldBe` []

testSoldierMoves:: Spec
testSoldierMoves = describe "IF Validate-Module-Catapult: soldierMoves ..." $ do
        it "wrong position" $ do
            soldierMoves sampleBoard White (Pos 'a' 9) `shouldBe` []

testCatapultMoves:: Spec
testCatapultMoves = describe "IF Validate-Module-Catapult: catapultMoves ..." $ do
        it "wrong position" $ do
            catapultMoves sampleBoard White (Pos 'a' 9) `shouldBe` []

testPlayerWon:: Spec
testPlayerWon = describe "IF Validate-Module-Catapult: playerWon ..." $ do
        it "nobody has won yet" $ do
            playerWon sampleBoard White `shouldBe` False

testListMoves:: Spec
testListMoves = describe "IF Validate-Module-Catapult: listMoves ..." $ do
        it "no moves" $ do
            listMoves noMoveBoard White `shouldBe` []