import HoofdCode
import Datastructures
import Graphics.Gloss

main :: IO ()
main = play window green fps game{board = rotatePile (generateGameStacks initBoard amountOfStacks) rotationAmount} render handleInput step
