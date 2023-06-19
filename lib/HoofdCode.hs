module HoofdCode where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy

--import Graphics.Glosstas.Interface.IO.Game
import System.Random ( mkStdGen )
import System.Random.Shuffle (shuffle')

import Datastructures
import Data.Char
import GHC.IO
import Data.Maybe



--explanation of my code :
-- mijn coordinatensysteem werkt als volgt, y=0 is de bovenste rij, x=0 is de meest linkse kolom, daarbuiten houd ik indexen bij van waar ik op de stack zit, dus selected index van selector is de index van de hoeveelste kaart op de stack geselecteerd is, en cardindex is de hoeveelste kaart op de stack waar de selector op zit
-- m verplaatst als mogelijk de geselecteerde stack/kaart naar de plaats waar de selector momenteel staat
-- r roteert door de pile per "rotationAmount" kaarten
-- s gooit de bovenste kaart van de pile op de eindstapel als dat mogelijk is
-- enter selecteert een stack/kaart
-- spatie gooit als mogelijk de geselecteerde kaart op de eindstapel
-- d neemt de bovenste kaart van de pile en legt deze onder de geselecteerde kaart als dat mogelijk is
-- pijltjes links en rechts bewgen vanzelfspreken naar links en rechts, pijltjes omhoog en omlaag bewegen naar boven en naar beneden in de stacks

amountOfEndingStacks :: Int
amountOfEndingStacks = 4

amountOfStacks :: Int
amountOfStacks = 7

amountOfCards :: Int
amountOfCards = 52

seed :: Int
seed = 45

fps :: Int
fps = 60

stackSpacing :: Int
stackSpacing = 20

xSpacing :: Int
xSpacing = 10


--height of window
height :: Int
height = 600

--width of window
width :: Int
width = 800

--size of card
cardHeight :: Int
cardHeight = 150

cardWidth :: Int
cardWidth = 100

rotationAmount :: Int
rotationAmount = 3

cardDeck :: Stack
cardDeck = shuffleList generateCards

--pile is voorlopig nog leeggelaten
initBoard :: Board
initBoard = Board {
    gameStacks = [],
    endingStacks = generateEndingStacks,
    pile = shuffleList generateCards
}

--index houd bij de hoeveelste kaart van de stack we op zitten
initSelector :: Selector
initSelector = Selector {
    position = (0,1),
    selected = Nothing,
    cardIndex = 0,
    selectedIndex = 0
}

game :: Game
game = Game {
    board = initBoard,
    selector = initSelector
}

windowPosition :: (Int, Int)
windowPosition = (200,200)

window :: Display
window = InWindow "Patience" (800, height) windowPosition


--map voor memory leak te vermijden
pictureMap :: [(Card,Picture)]
--pictureMap maps a card to a maybe picture
pictureMap = [(card,loadPicture t v)|card@(t,v,_) <- [(typ,val,Visible)|(typ,val,visible) <- generateCards]]

loadPicture :: CardType -> CardValue -> Picture
loadPicture t v = fromMaybe Blank (unsafePerformIO (loadJuicyPNG ("lib/assets/" ++ converValType t ++ "s/" ++ converValType t ++ "-" ++ convertVal v ++ ".png")))

lookupPicture :: Card -> Picture
lookupPicture c = fromJust (lookup c pictureMap)

symbols :: [Picture]
symbols = [loadsymbol "back" ,loadsymbol "placeholder", loadsymbol "selected", loadsymbol "selector"]

loadsymbol :: String ->Picture
loadsymbol val = fromMaybe Blank (unsafePerformIO (loadJuicyPNG ("lib/assets/" ++ val ++ ".png")))

convertVal:: CardValue -> String
convertVal val = case val of
    Ace -> "A"
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"

convertValNumber:: CardValue -> Int
convertValNumber val = case val of
    Ace -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 11
    Queen -> 12
    King -> 13

converValType :: CardType -> String
converValType typ = case typ of
    Spades -> "spade"
    Hearts -> "heart"
    Diamonds -> "diamond"
    Clubs -> "club"


generateEndingStacks :: [Stack]
generateEndingStacks = [[]| x <- [1..amountOfEndingStacks]]

shuffleList :: [a] -> [a]
shuffleList l = shuffle' l (length l) (mkStdGen seed)

generateCards :: [Card]
generateCards = [(x, y, Hidden)| x <- [Clubs .. Spades],  y <- [Ace .. King]]

--generates game stacks
generateGameStacks :: Board -> Int -> Board
generateGameStacks board n
    |n > 0 = generateGameStacks (generateStack board n) (n-1)
    |otherwise = board {gameStacks  = reverseList $ gameStacks board}

--reversesList
reverseList :: [w] -> [w]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]



generateStack :: Board -> Int -> Board
generateStack board n = board {pile = drop n (pile board), gameStacks = insert (gameStacks board) (amountOfStacks-n) (take n (pile board))}

--instert stack at location in list
insert :: [w] -> Int -> w -> [w]
insert stacks n stack = take n stacks ++ [stack] ++ drop (n+1) stacks

showCard :: Card -> Card
showCard (t, v, _) = (t, v, Visible)


--voor de convert functies is bv x=0 de meest linkse kaart op het scherm en y=0 de bovenste
convertX :: Int -> Float
convertX x = fromIntegral (x * (cardWidth + xSpacing)) - fromIntegral (width `div` 2) + fromIntegral (cardWidth `div` 2) + fromIntegral (xSpacing)

--n is de nummer van de kaart op de stack
convertY :: Int -> Int -> Float
convertY y n = (-1) * (fromIntegral (y * cardHeight) - fromIntegral (height `div` 2) + fromIntegral (cardHeight `div` 2) + fromIntegral (n * stackSpacing))
--convertY y n = fromIntegral(((y+1)*(cardHeight+20))) - (fromIntegral height) / 2 - (fromIntegral (n*stackSpacing))

--converteert kaart naar image, dus omgedraaid als hidden
cardImage :: Card -> Picture
cardImage (t, v, Hidden) = symbols!!0
cardImage card = lookupPicture card

--draait de onderste kaart van alle gamestacks naar visible
turnBottomCardGameStacks :: Board -> Board
turnBottomCardGameStacks board = board{gameStacks = [turnBottomCard x | x <- gameStacks board]}

--draait de onderste kaart van een stack om
turnBottomCard :: Stack -> Stack
turnBottomCard stack
    |length stack > 0 = (take (length stack - 1) stack) ++ [showCard (last stack)]
    |otherwise = stack

renderStack :: Stack -> Int -> Picture
renderStack stack n 
    | length stack > 0 = pictures [translate (convertX n) (convertY 1 x) (cardImage (stack!!x)) | x <- [0..((length stack)-1)]]
    | otherwise = translate (convertX n) (convertY 1 0) (symbols!!1)

renderGameStacks :: [Stack] -> Picture
renderGameStacks stacks = Pictures [renderStack (stacks!!x) x | x <- [0..((length stacks)-1)]]

--zie boven voor uitleg, roteert de pile
rotatePile :: Board -> Int -> Board
rotatePile board n
    | n > 1 = rotatePile (board {pile = next (pile board) ++ [(pile board)!!0]}) (n-1)
    | otherwise = board {pile = next (pile board) ++ [(showCard ((pile board)!!0))]}

moveDown :: Game -> Game
moveDown game
    | canMoveDown game = game {selector = (selector game){cardIndex = (cardIndex(selector game) + 1)}}
    | otherwise = game

canMoveDown :: Game -> Bool
canMoveDown game = index < (length ((gameStacks (board game)!!x)) - 1)
    where
        index = cardIndex (selector game)
        x = fst(position (selector game))

moveLeft :: Game -> Game
moveLeft game
    | canMoveLeft game && length ((gameStacks (board game))!!(fst (position (selector game)) - 1)) == 0 = game {selector = (selector game){position = (fst (position (selector game)) - 1, snd (position (selector game))), cardIndex = (length $ (gameStacks (board game))!!(fst (position (selector game)) - 1))}}
    | canMoveLeft game = game {selector = (selector game){position = (fst (position (selector game)) - 1, snd (position (selector game))), cardIndex = (length $ (gameStacks (board game))!!(fst (position (selector game)) - 1))-1}}
    | otherwise = game

canMoveLeft :: Game -> Bool
canMoveLeft game = fst (position (selector game)) > 0

moveRight :: Game -> Game
moveRight game
    | canMoveRight game = game {selector = (selector game){position = (fst (position (selector game)) + 1, snd (position (selector game))), cardIndex = (length $ (gameStacks (board game))!!(fst (position (selector game)) + 1))-1}}
    | otherwise = game

canMoveRight :: Game -> Bool
canMoveRight game = fst (position (selector game)) < amountOfStacks - 1

moveUp :: Game -> Game
moveUp game
    | canMoveUp game = game {selector = (selector game){cardIndex = (cardIndex (selector game)) - 1}}
    | otherwise = game


--seletor kan naar boven als de kaart daarboven visible is en er een kaart boven is
canMoveUp :: Game -> Bool
canMoveUp game =  index > 0 && (isVisible ((gameStacks (board game))!!(fst(position (selector game)))!!(index-1)))  where index = cardIndex (selector game)

isVisible :: Card -> Bool
isVisible (_, _, Visible) = True
isVisible _ = False

selectCard :: Game -> Game
selectCard game = game {selector = (selector game){selected = Just(fst(pos), snd(pos)), selectedIndex = cardIndex (selector game)}} where pos = position (selector game)

--geselecteerde kaart naar de endgameStacks doen
toFinalStack :: Game -> Game
toFinalStack game = 
    case selected (selector game) of
        Just (x, y)
            | length ((gameStacks (board game))!!(x)) > 0 -> toCorrectFinalStack (((gameStacks (board game))!!(x))!!(selectedIndex (selector game))) (x,y) game
            | otherwise -> game
        Nothing -> game

-- gooit een kaart als mogelijk naar de juiste endgame stack
toCorrectFinalStack :: (CardType, CardValue, CardStatus) -> Coordinate -> Game -> Game
toCorrectFinalStack (t, v, s) (stackNr, stackIndex) game = case t of
    Clubs
        | stackNr == -1 && (convertValNumber v == 1) && length ((endingStacks (board game))!!0) == 0 &&(convertValNumber v == 1) ->  game {board = (board game){endingStacks = insert (endingStacks (board game)) 0 [(t, v, s)], pile = next (pile (board game))}}
        | stackNr == -1 && (convertValNumber v == 1) && length ((endingStacks (board game))!!0) > 0 && (convertValNumber v == 1 + cardValue (((endingStacks (board game))!!0)!!0)) -> game {board = (board game){endingStacks = insert (endingStacks (board game)) 0 ((endingStacks (board game))!!0 ++ [(t, v, s)]), pile = next (pile (board game))}}
        | stackNr > -1 && length ((endingStacks (board game))!!0) == 0 && (convertValNumber v == 1) -> nextGame $ removeSelected  (game {board = (board game){endingStacks = insert (endingStacks (board game)) 0 [(t, v, s)]}})
        | stackNr > -1 && length ((endingStacks (board game))!!0) > 0 && (convertValNumber v == 1 + cardValue (((endingStacks (board game))!!0)!!0)) -> nextGame $ removeSelected(game {board = (board game){endingStacks = insert (endingStacks (board game)) 0 [(t, v, s)]}})
        | otherwise -> game
    Diamonds
        | stackNr == -1 && (convertValNumber v == 1) && length ((endingStacks (board game))!!1) == 0 && (convertValNumber v == 1) ->  game {board = (board game){endingStacks = insert (endingStacks (board game)) 1 [(t, v, s)], pile = next (pile (board game))}}
        | stackNr == -1 && (convertValNumber v == 1) && length ((endingStacks (board game))!!1) > 0 && (convertValNumber v == 1 + cardValue (((endingStacks (board game))!!1)!!0)) -> game {board = (board game){endingStacks = insert (endingStacks (board game)) 1 ((endingStacks (board game))!!1 ++ [(t, v, s)]), pile = next (pile (board game))}}
        | stackNr > -1 && length ((endingStacks (board game))!!1) == 0 && (convertValNumber v == 1) -> nextGame $ removeSelected (game {board = (board game){endingStacks = insert (endingStacks (board game)) 1 [(t, v, s)]}})
        | stackNr > -1 && length ((endingStacks (board game))!!1) > 0 && (convertValNumber v == 1 + cardValue (((endingStacks (board game))!!1)!!0)) -> nextGame $ removeSelected (game {board = (board game){endingStacks = insert (endingStacks (board game)) 1 [(t, v, s)]}})
        | otherwise -> game
    Hearts
        | stackNr == -1 && (convertValNumber v == 1) && length ((endingStacks (board game))!!2) == 0 &&  (convertValNumber v == 1) ->  game {board = (board game){endingStacks = insert (endingStacks (board game)) 2 [(t, v, s)], pile = next (pile (board game))}}
        | stackNr == -1 && (convertValNumber v == 1) && length ((endingStacks (board game))!!2) > 0 && (convertValNumber v == 1 + cardValue (((endingStacks (board game))!!2)!!0)) -> game {board = (board game){endingStacks = insert (endingStacks (board game)) 2 ((endingStacks (board game))!!2 ++ [(t, v, s)]), pile = next (pile (board game))}}
        | stackNr > -1 && length ((endingStacks (board game))!!2) == 0 && (convertValNumber v == 1) -> nextGame $ removeSelected  (game {board = (board game){endingStacks = insert (endingStacks (board game)) 2 [(t, v, s)]}})
        | stackNr > -1 && length ((endingStacks (board game))!!2) > 0 && (convertValNumber v == 1 + cardValue (((endingStacks (board game))!!2)!!0)) -> nextGame $ removeSelected(game {board = (board game){endingStacks = insert (endingStacks (board game)) 2 [(t, v, s)]}})
        | otherwise -> game
    Spades
        | stackNr == -1 && (convertValNumber v == 1) && length ((endingStacks (board game))!!2) == 0 && (convertValNumber v == 1) ->  game {board = (board game){endingStacks = insert (endingStacks (board game)) 3 [(t, v, s)], pile = next (pile (board game))}}
        | stackNr == -1 && (convertValNumber v == 1) && length ((endingStacks (board game))!!2) > 0 && (convertValNumber v == 1 + cardValue (((endingStacks (board game))!!3)!!0)) -> game {board = (board game){endingStacks = insert (endingStacks (board game)) 3 ((endingStacks (board game))!!3 ++ [(t, v, s)]), pile = next (pile (board game))}}
        | stackNr > -1 && length ((endingStacks (board game))!!3) == 0 && (convertValNumber v == 1) -> nextGame $ removeSelected  (game {board = (board game){endingStacks = insert (endingStacks (board game)) 3 [(t, v, s)]}})
        | stackNr > -1 && length ((endingStacks (board game))!!3) > 0 && (convertValNumber v == 1 + cardValue (((endingStacks (board game))!!3)!!0)) -> nextGame $ removeSelected(game {board = (board game){endingStacks = insert (endingStacks (board game)) 3 [(t, v, s)]}})
        | otherwise -> game

cardValue :: Card -> Int
cardValue (_, v, _) = convertValNumber v

-- gebruikt met pile, gooit 1e kaart weg en maakt 2e visisble 
next :: [Card] -> [Card]
next l = [showCard (l!!1)] ++ (drop 2 l)

-- removes selected card from the board
removeSelected :: Game -> Game
removeSelected game = case selected (selector game) of
    Just (x, y)
        |(x,y) == position (selector game) -> game {board = (board game){gameStacks = (take (x) (gameStacks (board game))) ++ [take (selectedIndex (selector game)) (gameStacks (board game)!!x)] ++ (drop (x+1) (gameStacks (board game)))}, selector = (selector game){selected = Nothing, cardIndex = cardIndex (selector game) - 1}}
        | otherwise -> game {board = (board game){gameStacks = (take (x) (gameStacks (board game))) ++ [take (selectedIndex (selector game)) (gameStacks (board game)!!x)] ++ (drop (x+1) (gameStacks (board game)))}, selector = (selector game){selected = Nothing}}
    Nothing -> game

-- kaart van pile naar selected kaart/stack
drawCard :: Game -> Game
drawCard game = case selected (selector game) of
    Just (x, y)
        | length ((gameStacks (board game))!!(fst(position (selector game)))) == 0 && isKing (head (pile(board game))) -> game{board = (board game){gameStacks = insert (gameStacks (board game)) (fst(position (selector game))) [(head (pile(board game)))], pile = [showCard (pile(board game)!!1)] ++ drop 2 (pile(board game))},  selector = (selector game){selected = Nothing}}
        | length ((gameStacks (board game))!!(fst(position (selector game)))) == 0 -> game
        | validDraw (head (pile(board game))) (cardAt game (x,(selectedIndex (selector game)))) -> game {board = (board game){gameStacks = (take (x) (gameStacks (board game))) ++ [(gameStacks (board game)!!x) ++ [(head (pile (board game)))]] ++ (drop (x+1) (gameStacks (board game))), pile = [showCard (pile(board game)!!1)] ++ drop 2 (pile(board game))},  selector = (selector game){selected = Nothing}}
        | otherwise -> game
    Nothing -> game

--gaat na of een kaart past op een andere kaart
validDraw :: Card -> Card -> Bool
validDraw (t1, v1, s1) (t2, v2, s2) = (convertValNumber v1 == (convertValNumber v2 - 1)) && (not (sameColor t1 t2))

--checkt of 2 kaarten zelfde kleur zijn
sameColor :: CardType -> CardType -> Bool
sameColor Hearts Diamonds = True
sameColor Diamonds Hearts = True
sameColor Clubs Spades = True
sameColor Spades Clubs = True
sameColor _ _ = False


remove :: [w] -> Int -> [w]
remove l index = (take (index) l) ++ (drop (index+1) l)



handleInput :: Event -> Game -> Game
--zie commentaar vanboven voor uitleg van besturing
handleInput ev game
    | isKey KeyUp ev = nextGame (moveUp game)
    | isKey KeyLeft ev = nextGame (moveLeft game)
    | isKey KeyRight ev = nextGame (moveRight game)
    | isKey KeyDown ev = nextGame (moveDown game)
    | isKey KeyEnter ev = nextGame (selectCard game)
    | isKey KeySpace ev = nextGame (toFinalStack game)
    | isCharKey 'r' ev = nextGame (game{board = rotatePile (board game) rotationAmount})
    | isCharKey 'd' ev = nextGame (drawCard game)
    | isCharKey 'm' ev = nextGame (moveCard game)
    | isCharKey 's' ev = nextGame (pileToFinalStack game)
    | otherwise = game

--pass it with -1 as stacknr to indicate it's from the pile
pileToFinalStack :: Game -> Game
pileToFinalStack game = toCorrectFinalStack (head (pile (board game))) (-1,0) game{board = (board game)}


moveCard :: Game -> Game
moveCard game = case selected (selector game) of
    Just (x, y)
        | length ((gameStacks (board game))!!x) == 0 -> game
        | length ((gameStacks (board game))!!(fst(position (selector game)))) == 0 && isKing (cardAt game (x,(selectedIndex (selector game)))) -> removeSelected game{board = (board game){gameStacks = insert (gameStacks (board game)) (fst(position (selector game))) [(cardAt game (x,(selectedIndex (selector game))))]}}
        | length ((gameStacks (board game))!!(fst(position (selector game)))) == 0 -> game
        | validDraw (cardAt game (x,(selectedIndex (selector game)))) (cardAt game ((fst pos), (cardIndex (selector game)))) -> removeSelected (game {board = (board game){gameStacks = take (fst pos) (gameStacks (board game)) ++ [ addStack ((gameStacks (board game))!!(fst pos)) (drop (selectedIndex (selector game)) ((gameStacks(board game))!!x))] ++ drop ((fst pos) + 1) (gameStacks (board game))}})
        | otherwise -> game where pos = position (selector game)
    Nothing -> game

isKing :: Card -> Bool
isKing (_, v, _) = v == King

isAce :: Card -> Bool
isAce (_, v, _) = v == Ace

--moves everything from indexToMove from indexSubStack to last to indexMoveTo stack
moveStack :: Game -> Int -> Int -> Int -> Game
moveStack game indexToMove indexSubStack indexMoveTo = game{ board = (board game){gameStacks = []}}

addStack :: Stack -> Stack -> Stack
addStack [] stack = stack
addStack stack [] = stack
addStack (x:xs) stack = x : addStack xs stack

addToStack :: Stack -> Card -> Stack
addToStack stack card = stack ++ [card]

--returns the card at the given position, y is de nummer v/d kaart op de stack, x is de index van de stack zelf
cardAt :: Game -> Coordinate -> Card
cardAt game (x,y) = (gameStacks (board game)!!x)!!y

-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey _  _                                   = False

-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isCharKey :: Char -> Event -> Bool
isCharKey k1 (EventKey (Char k2) Down _ _) = k1 == k2
isCharKey _  _                                   = False

renderPile :: Stack -> Picture
renderPile pile = translate (convertX 0) (convertY 0 0) (cardImage (pile!!0))

renderSelector :: Selector -> Picture
renderSelector selector =
    case (selected selector) of
        Nothing -> translate (convertX (fst (position selector))) (convertY (snd (position selector)) (cardIndex selector)) (symbols!!3)
        Just (x, y) -> Pictures [translate (convertX x) (convertY y (selectedIndex selector)) (symbols!!2), translate (convertX (fst (position selector))) (convertY (snd (position selector)) (cardIndex selector)) (symbols!!3)]

renderEndingStacks :: [Stack] -> Picture
renderEndingStacks endingStacks = Pictures [translate (convertX (x+3)) (convertY 0 0) (renderEndingStack (endingStacks!!x)) | x <- [0..((length endingStacks)-1)]]

renderEndingStack :: Stack -> Picture
renderEndingStack stack
    | length stack > 0 = cardImage (stack!!0)
    | otherwise = symbols!!1

endingStackPicture :: Stack -> Picture
endingStackPicture stack
    |length stack == 0 = symbols!!1
    |otherwise = cardImage (last stack)

nextGame :: Game -> Game
nextGame game = game {board = turnBottomCardGameStacks (board game)}


render :: Game -> Picture
render game = Pictures[renderGameStacks (gameStacks (turnBottomCardGameStacks (board game))), renderSelector (selector game), renderPile (pile (board game)), renderEndingStacks (endingStacks (board game))]

step :: Float -> Game -> Game
step _ b = b