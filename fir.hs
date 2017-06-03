import qualified Data.Map as Map
import qualified Data.Tree as Tree
import Data.List

data Color = B | W deriving Eq
newtype Position = Pos (Int, Int) deriving (Show, Eq)
newtype Board = Board(Map.Map Position Color) deriving Eq
data Game = Null | Game Board Color Position deriving (Show, Eq)--Int
data Direction = Vertical | Horizontal | DiagonalR | DiagonalL deriving Eq

instance Ord Position where
    compare (Pos(l1,r1)) (Pos(l2,r2))
        | l1 == l2 && r1==r2 = EQ
        | l1 == l2 && r1<r2 = LT 
        | l1 == l2 && r1>r2 = GT
        | l1 < l2 = LT
        | l1 > l2 = GT

instance Show Color where
    show B = "B"                --"⚈"
    show W = "W"                --"⚆"

mapRows = 19
cords = [1..mapRows]

instance Show (Board) where
    show x = showBoard x


numberString = "x\\y  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 "
showBoard :: (Board) -> [Char]
showBoard (Board m) = ("\n"++numberString++intercalate "" [((nextRow y x) ++ (showCell m (Pos(x, y))))| x <-cords, y<-cords])++"\n"

showCell m pos
    | Map.lookup pos m == Nothing = " - "                   --" ⊹ "
    | Map.lookup pos m == Just B = " "++ (show B) ++" "
    | Map.lookup pos m == Just W = " "++ (show W) ++" "


nextRow y x
    | y /= 1 = ""
    | y == 1 && x<10 = "\n "++(show x)++"  "
    | y == 1 && x>=10 = "\n "++(show x)++" "
getMap (Board(m)) = m



-- #lab4

--Game -> [Game]
--f: Board->C -> [Board]


getOppositeColor c 
    | c == B = W
    | c == W = B

insertCell pos c (Board map) = Board (Map.insert pos c map)

posibleMoves (Game board color pos) = [(Game (insertCell (Pos(x,y)) color board) (getOppositeColor color) (Pos(x,y)))| x<-cords, y<-cords, not(checkIfExists board (Pos(x,y)))]

createTree game = Tree.Node game (map createTree (posibleMoves game))



--TODO explore function

--exploreTree (Tree.Node _ []) _ = Null


{-
exploreTree (Tree.Node v c) game
    | game == v = chooseBestOption c--(getValueList c)--map evaluationFunction  (getValueList c) -- chooseBestOption c
    | game /= v = Null

chooseBestOption list = (getValueList list) !! (mg
axIndex (map evaluationFunction (getValueList list)))


exploreTreeMax :: Tree.Tree Game -> Tree.Tree Game
exploreTreeMax (Tree.Node v c) = chooseMaxOption c
exploreTreeMin (Tree.Node v c) = chooseMinOption c

exploreTree2 t = exploreTreeMin (exploreTreeMax t)

chooseMinOption :: [Tree.Tree Game] -> Tree.Tree Game
chooseMaxOption list = list !! (maxIndex (map evaluationFunction (getValueList list)))
chooseMinOption list = list !! (minIndex (map (\x -> -(evaluationFunction x)) (getValueList list)))

-}

-- exploreTree :: Tree.Tree Game -> Tree.Tree Game
    -- exploreTree (Tree.Node v c) = --chooseMaxOption c



-- mocne TODO    
tmp = getOptions iTree

getChildren (Tree.Node _ list) = [map getChildren2 list]
getChildren2 (Tree.Node _ list) = list

getOptions (Tree.Node _ list) = map chooseOptionEnd list  
chooseOptionEnd (Tree.Node _ list) = maximum (map evaluationFunction (getValueList list))
chooseOptionMin list = minimum list
chooseOptionMax list = maximum list





--chooseMinOption list = list !! (minIndex (map (\x -> -(evaluationFunction x)) (getValueList list)))
get2thChild (Tree.Node v c) = []





getValueList list = map (\x -> getValueFromTree x) list
getValueFromTree (Tree.Node v _) = v

maxIndex list = snd . maximumBy cmp $ zip list [0 .. ] where
    cmp (v,i) (w,j) = case compare v w of
                        EQ -> compare j i
                        ne -> ne

minIndex list = snd . minimumBy cmp $ zip list [0 .. ] where
    cmp (v,i) (w,j) = case compare v w of
                        EQ -> compare j i
                        ne -> ne


--INITIAL--
iMap = Map.empty
iBoard = Board iMap
iGame = Game iBoard W (Pos(0,0))
iTree = createTree iGame
--END--


--TESTS--
testMap1 = Map.fromList ([(Pos(x,y), W)| x <-[8..13], y<-[4..7]]++[(Pos(12,8), W),(Pos(12,3), W)]++[(Pos(7,4), B)])
testMap2 = Map.fromList ([(Pos(5+k,6+k), B)| k <-[0..3]]++[(Pos(5+k,4+k), B)| k <-[0..3]]++[(Pos(5+k,5+k), W)| k <-[0..4]])

testMap3 = Map.fromList ([(Pos(13,4), W),(Pos(13,3), W),(Pos(13,2), W),(Pos(12,4), W),(Pos(12,3), B)])
testBoard3 = Board(testMap3)
testGame3 = Game testBoard3 B (Pos(12,3))

testMap4 = Map.fromList ([(Pos(1,1), B)])
testBoard4 = Board(testMap4)
testGame4 = Game testBoard4 B (Pos(1,1))

testMap5 = Map.fromList ([(Pos(16+k,6), B)| k <-[0..3]]++[(Pos(15,6), W)])
testBoard5 = Board(testMap5)
testGame5 = Game testBoard5 B (Pos(16,6))


--tmp =  getValueFromTree (exploreTree $ exploreTree $ exploreTree $ exploreTree $ exploreTree $ exploreTree iTree)

--END--




--evaluationFunction :: Game -> Int
changeColorInGame (Game map color pos) = Game map (getOppositeColor color) pos

evaluationFunction game
    | isOptionA1 (changeColorInGame game) = 100
    | isOptionA2 (changeColorInGame game) = 90
    | isOptionA3 (changeColorInGame game) = 85
    | isOptionA4 (changeColorInGame game) = 40
    | isOptionA5 (changeColorInGame game) = 38
    | isOptionA6 (changeColorInGame game) = 30
    | isOptionA7 (changeColorInGame game) = 28
    | isOptionA8 (changeColorInGame game) = 20
    | isOptionA9 (changeColorInGame game) = 15
    | isOptionA10 (changeColorInGame game) = 10
    | isOptionA11 (changeColorInGame game) = 5
    | isOptionA12 (changeColorInGame game) = 1
    | otherwise = 0

--5 in line 
isOptionA1 game = head (inLine 5 game) /= [] 
--4 in line (1 empty)
isOptionA2 game = head (inLine 4 game) /= [] && length (last (inLine 4 game))>0 
--3 in line (2 empty)
isOptionA3 game = head (inLine 3 game) /= [] && length (last (inLine 3 game))>1 
--3 in line (1 empty)
isOptionA4 game = head (inLine 3 game) /= [] && length (last (inLine 3 game))>0 
--2 in line (2 empty)
isOptionA5 game = head (inLine 2 game) /= [] && length (last (inLine 2 game))>1 
--2 in line (1 empty)
isOptionA6 game = head (inLine 2 game) /= [] && length (last (inLine 2 game))>0 
--1 in line (2 empty)
isOptionA7 game = head (inLine 1 game) /= [] && length (last (inLine 1 game))>1 
--1 in line (1 empty)
isOptionA8 game = head (inLine 1 game) /= [] && length (last (inLine 1 game))>0 
--4 in line (0 empty)
isOptionA9 game = head (inLine 4 game) /= []  
--3 in line (0 empty)
isOptionA10 game = head (inLine 3 game) /= [] 
--2 in line (0 empty)
isOptionA11 game = head (inLine 2 game) /= [] 
--1 in line (0 empty)
isOptionA12 game = head (inLine 1 game) /= []  




inLine n game -- = True
    | head (checkVerticalInArea n game) /= [] = checkVerticalInArea n game
    | head (checkHorizontalInArea n game) /= [] = checkHorizontalInArea n game
    | head (checkDiagonalRInArea n game) /= [] = checkDiagonalRInArea n game 
    | head (checkDiagonalLInArea n game) /= [] = checkDiagonalLInArea n game
    | otherwise = [[], [], []]

checkVerticalInArea n (Game (Board map) color (Pos(x,y))) = getMainResultTable [(inDirection Vertical n map color a y) | a<-[(x-n+1)..(x)]] Vertical n map color (Pos(x,y))
checkHorizontalInArea n (Game (Board map) color (Pos(x,y))) = getMainResultTable [(inDirection Horizontal n map color x b) | b<-[(y-n+1)..(y)]] Horizontal n map color (Pos(x,y))
checkDiagonalRInArea n (Game (Board map) color (Pos(x,y))) = getMainResultTable [(inDirection DiagonalR n map color (x-k) (y-k)) | k<-[0..(n-1)]] DiagonalR n map color (Pos(x,y))
checkDiagonalLInArea n (Game (Board map) color (Pos(x,y))) = getMainResultTable [(inDirection DiagonalL n map color (x-k) (y-k)) | k<-[0..(n-1)]] DiagonalL n map color (Pos(x,y))

getMainResultTable list direction n map color pos = [getFromListIndex list, getOpponentsPos list direction map color, getEmptyPos list direction map color]

getOpponentsPos list direction map color = filter (\lambda -> ((checkIfColor map lambda (getOppositeColor color)))) (getEdgePos direction (head (getFromListIndex list)) (last (getFromListIndex list)))
getEmptyPos list direction map color =  filter (\lambda -> ((not ((checkIfExists (Board map) lambda))) && (isOnBoard lambda))) (getEdgePos direction (head (getFromListIndex list)) (last (getFromListIndex list)))


getEdgePos direction (Pos(fx,fy)) (Pos(lx,ly))
    | (direction == Vertical) = [Pos(fx-1,fy), Pos(lx+1,ly)]
    | (direction == Horizontal) = [Pos(fx,fy-1), Pos(lx,ly+1)]
    | (direction == DiagonalR) = [Pos(fx-1,fy-1), Pos(lx+1,ly+1)]
    | (direction == DiagonalL) = [Pos(fx-1,fy+1), Pos(lx+1,ly-1)]

inDirection direction n map color a b 
    | (direction == Vertical) && (and [(checkIfColor map (Pos(x, b)) color) | x<-[a..(a+n-1)]]) = sort [(Pos(x, b)) | x<-[a..(a+n-1)]]
    | (direction == Horizontal) && (and [(checkIfColor map (Pos(a, y)) color) | y<-[b..(b+n-1)]]) = sort [(Pos(a, y)) | y<-[b..(b+n-1)]]
    | (direction == DiagonalR) && (and [(checkIfColor map (Pos(a+k, b+k)) color) | k<-[0..n-1]]) = sort [Pos(a+k, b+k) | k<-[0..n-1]]
    | (direction == DiagonalL) && (and [(checkIfColor map (Pos(a-k, b+k)) color) | k<-[0..n-1]]) = sort [Pos(a-k, b+k) | k<-[0..n-1]]
    | otherwise = []

getFromListIndex list 
    | getIndexOfPositions list == Nothing = []
    | getIndexOfPositions list /= Nothing = list !! getIndexFromJust (getIndexOfPositions list)

getIndexFromJust (Just index) = index
getIndexOfPositions list = findIndex (\x-> length x > 0) list
checkIfColor map pos color = (Map.lookup pos map == Just color) && (isOnBoard pos)
checkIfExists (Board map) pos = isOnBoard pos && (Map.lookup pos map /= Nothing)   
isOnBoard (Pos(x,y)) = x > 0 && x <= mapRows && y > 0 && y <= mapRows 

