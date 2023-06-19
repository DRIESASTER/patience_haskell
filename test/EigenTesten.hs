import Test.Hspec
import Datastructures
import HoofdCode


main :: IO ()
main = hspec $ do
    it "should return a board with 7 stacks" $ do
        length (gameStacks (generateGameStacks initBoard amountOfStacks)) `shouldBe` 7
    it "should return a board with 4 ending stacks" $ do
        length (endingStacks (generateGameStacks initBoard amountOfStacks)) `shouldBe` 4
    it "should return a board with 24 cards in the pile" $ do
        length (pile (generateGameStacks initBoard amountOfStacks)) `shouldBe` 24
    it "should return a board with 1 card in the first stack" $ do
        length (head (gameStacks (generateGameStacks initBoard amountOfStacks))) `shouldBe` 1
    it "should return a board with 6 cards in the sixth stack" $ do
        length (head (tail (tail (tail (tail (tail (gameStacks (generateGameStacks initBoard amountOfStacks)))))))) `shouldBe` 6
    it "should return a board with 0 cards in the ending stacks" $ do
        length (head (endingStacks (generateGameStacks initBoard amountOfStacks))) `shouldBe` 0