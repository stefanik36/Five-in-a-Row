import qualified Data.Map as Map
import qualified Data.Tree as Tree
import Data.List


--ADDED--
import System.Environment
import Control.Monad.Trans.State.Lazy
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Number
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Error
import System.IO
import Data.Maybe
import Control.Monad.IO.Class
import Data.Char
import Data.Maybe
--END--





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

mapRows = 8 
cords = [1..mapRows]

instance Show (Board) where
    show x = showBoard x


numberString = "x\\y " ++ intercalate "" (map (\x -> " "++show x++" ") [1..mapRows])--"x\\y  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 "
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






getNextTree (Tree.Node v list) pos = (filter (\x -> pos == (getNewPositionFromGame (getValueFromTree x))) list)!!0

getNewPositionFromGame (Game board color pos) = pos
move tree pos = getValueFromTree (getNextTree tree pos)




getTree [] pos = getNextTree iTree pos
getTree (x:xs) pos = getNextTree (getTree xs $ getNewPositionFromGame x) pos

getTreeValue l pos = getValueFromTree $ getTree l pos 


getBoardFromGame (Game board _ _) = board
getColorFromGame (Game _ color _) = color


----------------------------------------------------- Player Vs Player ------------------------------------------
doPvp = pvp iTree

pvp tree = do
    i <- getLine
    case parse parsePos "parse error" (i) of
        Right p -> do
            hPutStrLn stderr $ "ruch = " ++ (show (Pos p))
            hPutStrLn stdout $ show $ getBoardFromGame $ getValueFromTree $ getNextTree tree (Pos p)
            pvp $ getNextTree tree (Pos p)
        Left x -> fail $ show x



----------------------------------------------------- Player Vs CPU ------------------------------------------
doPvcpu = pvcpu iTree

checkEnd game = ((evaluationFunction game) == 100)
showWinner game = hPutStrLn stdout $ "won: " ++ (show $ getOppositeColor $ getColorFromGame game)


pvcpu tree = 
        if(checkEnd $ getValueFromTree tree) then showWinner $ getValueFromTree tree else do
        hPutStrLn stdout $ show $ getBoardFromGame $ getValueFromTree $ tree
        i <- getLine
        case parse parsePos "parse error" (i) of
            Right p -> do
                hPutStrLn stderr $ "ruch = " ++ (show (Pos p))
                hPutStrLn stdout $ show $ getBoardFromGame $ getValueFromTree $ getNextTree tree (Pos p)
                if (checkEnd $ getValueFromTree $ getNextTree tree (Pos p)) then showWinner $ getValueFromTree $ getNextTree tree (Pos p) else do
                pvcpu $ cpuMove $ getNextTree tree (Pos p)
            Left x -> fail $ show x


--NEW FEATURES --
--- cabal: parsec3, parsec-numbers



parsePosI :: Parser Int
parsePosI = do
            x <- int
            if (x<1 || x>mapRows) then
              unexpected "Tylko liczby od 1-19"
            else
              return x

parsePosC :: Parser Int
parsePosC = do
            x <- lower
            if (x<'a' || x>'z') then
              unexpected "Tylko znaki od a-z"
            else
              return $ (ord x) - (ord 'a') + 1


parseSinglePos =  choice [parsePosI,parsePosC]
parsePos = do
              x <- parseSinglePos
              spaces
              y <- parseSinglePos
              return (x,y)


doPlay = getContents >>= (mapM_ play) . lines


play i = do
  case parse parsePos "Parse error" i of
    -- Right pos -> (hPutStrLn stdout $ "ruch = " ++ (show (move (Pos pos))) ++ (show $ move $ getNewPositionFromGame $ getChildrenMax1 $ getNextTree testTree3 (Pos pos))) -- 
    Right pos -> (hPutStrLn stderr $ "ruch = " ++ (show  (Pos pos)))
    Left x -> fail $ show x


main = doPlay






--INITIAL--
iMap = Map.empty
iBoard = Board iMap
iGame = Game iBoard W (Pos(0,0))
iTree = createTree iGame
--END--


--TESTS--
testMap1 = Map.fromList ([(Pos(x,y), W)| x <-[8..13], y<-[4..7]]++[(Pos(12,8), W),(Pos(12,3), W)]++[(Pos(7,4), B)])
testMap2 = Map.fromList ([(Pos(5+k,6+k), B)| k <-[0..3]]++[(Pos(5+k,4+k), B)| k <-[0..3]]++[(Pos(5+k,5+k), W)| k <-[0..4]])

testMap3 = Map.fromList ([(Pos(1,4), W),(Pos(1,3), W),(Pos(1,2), W),(Pos(2,4), W),(Pos(2,3), B)])
testBoard3 = Board(testMap3)
testGame3 = Game testBoard3 B (Pos(2,3))
testTree3 = createTree testGame3

testMap4 = Map.fromList ([(Pos(1,1), B)])
testBoard4 = Board(testMap4)
testGame4 = Game testBoard4 B (Pos(1,1))

testMap6 = Map.fromList ([(Pos(1,2), W)])
testBoard6 = Board(testMap6)
testGame6 = Game testBoard6 W (Pos(1,2))


testMap7 = Map.fromList ([(Pos(1,3), B)])
testBoard7 = Board(testMap7)
testGame7 = Game testBoard7 B (Pos(1,3))



testMap5 = Map.fromList ([(Pos(16+k,6), B)| k <-[0..3]]++[(Pos(15,6), W)])
testBoard5 = Board(testMap5)
testGame5 = Game testBoard5 B (Pos(16,6))


--tmp =  getValueFromTree (exploreTree $ exploreTree $ exploreTree $ exploreTree $ exploreTree $ exploreTree iTree)

--END--





{-
printHistory :: Show a => [a] -> IO ()
printHistory h =  do
  hPutStrLn stderr "History"
  mapM_ (hPutStrLn stderr.show) h

type Play a = StateT [(Int,Int)] IO a
playS :: String -> Play ()
playS i = do
  history <- get
  case parse parsePos "Parse error" i of
    Right  pos->
              let newHistory = pos:history
              in (liftIO $ hPutStrLn stderr $ "ruch = " ++ (show pos))
                 >> put newHistory
    Left _ -> fail ("koniec")
  liftIO $ printHistory history


doPlayS = liftIO getContents >>= (mapM_ playS) . lines

mainS = evalStateT doPlayS []
-}
-- main = putStrLn $ show $ length $ levels $ gameTree $ initGame 3

--END--






























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


createTreeInt i = Tree.Node i (map createTreeInt [1..3])



{-
getChildrenMax1 (Tree.Node _ list) = (map getChildrenMin2 list)
getChildrenMin2 (Tree.Node _ list) = (map getChildrenMax3 list)
getChildrenMax3 (Tree.Node _ list) =  (map evaluationFunction (getValueList list))-- getValueList list--maximum (map evaluationFunction (getValueList list))



getChildrenMax 0 (Tree.Node _ list) = (map evaluationFunction (getValueList list))-- getValueList list--maximum (map evaluationFunction (getValueList list))
getChildrenMax n (Tree.Node _ list) = (map (\x -> getChildrenMin (n-1) x) list)

getChildrenMin 0 (Tree.Node _ list) = (map evaluationFunction (getValueList list))-- getValueList list--maximum (map evaluationFunction (getValueList list))
getChildrenMin n (Tree.Node _ list) = (map (\x -> getChildrenMax (n-1) x) list)
-}



getSubTree 0 v = Tree.Node v [] 



-- getSubTree n v = Tree.Node n (map getSubTree (n-1) v)



--drawVerticalTree 

cpuMove tree = gcm1 tree

gcm1 (Tree.Node _ list) = list !! minIndex (map gcm2 list)--(map getValueFromTree list) !! maxIndex (map gcm2 list)
gcm2 (Tree.Node _ list) = maximum (map evaluationFunction (getValueList list)) --(map gcm3 list)
gcm3 (Tree.Node _ list) = minimum (map evaluationFunction (getValueList list))


getChildrenMax1 (Tree.Node _ list) = list !! maxIndex (map getChildrenMin2 list)
getChildrenMin2 (Tree.Node _ list) = minimum (map getChildrenMax3 list)--(map evaluationFunction (getValueList list))--(map getChildrenMax3 list)
getChildrenMax3 (Tree.Node _ list) = maximum (map evaluationFunction (getValueList list))-- getValueList list--maximum (map evaluationFunction (getValueList list))



{-}
getChildren(n Tree.Node _ list)
    | n==0 = (map getValueFromTree list) !! maxIndex (map get) 

--getChildrenNew (Tree.Node _ list) = map [] list

-}




getOptions (Tree.Node _ list) = map chooseOptionEnd list  
chooseOptionEnd (Tree.Node _ list) = maximum (map evaluationFunction (getValueList list))

tmp2 (Tree.Node _ list) =minimum (map (\(Tree.Node _ l)-> maximum (map evaluationFunction (getValueList l))) list)

{-
getOptions (Tree.Node _ list) = map chooseOptionEnd list  
chooseOptionEnd (Tree.Node _ list) = maximum (map evaluationFunction (getValueList list))
chooseOptionMin list = minimum list
chooseOptionMax list = maximum list
-}
--profiler

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
    | isOptionA8 (changeColorInGame game) = -20
    | isOptionA9 (changeColorInGame game) = -15
    | isOptionA10 (changeColorInGame game) = -10
    | isOptionA11 (changeColorInGame game) = -5
    | isOptionA12 (changeColorInGame game) = -1
    | otherwise = 0


    --io var 

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



{-

checkColumnInArea n map color (Pos(x,y)) = [getFromListIndex [(inColumn n map color a y) | a<-[(x-n+1)..(x)]], filter (\x -> ((checkIfColor map x (getOppositeColor color)))) (getEdgePosColumn (head (getFromListIndex [(inColumn n map color a y) | a<-[(x-n+1)..(x)]])) (last (getFromListIndex [(inColumn n map color a y) | a<-[(x-n+1)..(x)]]))), filter (\x -> (not ((checkIfExists (Board map) x)))) (getEdgePosColumn (head (getFromListIndex [(inColumn n map color a y) | a<-[(x-n+1)..(x)]])) (last (getFromListIndex [(inColumn n map color a y) | a<-[(x-n+1)..(x)]])))]

checkRowInArea n map color (Pos(x,y)) = [getFromListIndex [(inRow n map color x b) | b<-[(y-n+1)..(y)]], filter (\x -> ((checkIfColor map x (getOppositeColor color)))) (getEdgePosRow (head (getFromListIndex [(inRow n map color x b) | b<-[(y-n+1)..(y)]])) (last (getFromListIndex [(inRow n map color x b) | b<-[(y-n+1)..(y)]]))), filter (\x -> (not ((checkIfExists (Board map) x)))) (getEdgePosRow (head (getFromListIndex [(inRow n map color x b) | b<-[(y-n+1)..(y)]])) (last (getFromListIndex [(inRow n map color x b) | b<-[(y-n+1)..(y)]])))]

checkRightDiagonalInArea n map color (Pos(x,y)) = [getFromListIndex [(inRightDiagonal n map color (x-k) (y-k)) | k<-[0..(n-1)]], filter (\x -> ((checkIfColor map x (getOppositeColor color)))) (getEdgePosColumn (head (getFromListIndex [(inRightDiagonal n map color (x-k) (y-k)) | k<-[0..(n-1)]])) (last (getFromListIndex [(inRightDiagonal n map color (x-k) (y-k)) | k<-[0..(n-1)]]))), filter (\x -> (not ((checkIfExists (Board map) x)))) (getEdgePosColumn (head (getFromListIndex [(inRightDiagonal n map color (x-k) (y-k)) | k<-[0..(n-1)]])) (last (getFromListIndex [(inRightDiagonal n map color (x-k) (y-k)) | k<-[0..(n-1)]])))]

checkLeftDiagonalInArea n map color (Pos(x,y)) = [getFromListIndex [(inLeftDiagonal n map color (x+k) (y-k)) | k<-[0..(n-1)]], filter (\x ->  ((checkIfColor map x (getOppositeColor color)))) (getEdgePosColumn (head (getFromListIndex [(inLeftDiagonal n map color (x+k) (y-k)) | k<-[0..(n-1)]])) (last (getFromListIndex [(inLeftDiagonal n map color (x+k) (y-k)) | k<-[0..(n-1)]]))), filter (\x -> (not ((checkIfExists (Board map) x)))) (getEdgePosColumn (head (getFromListIndex [(inLeftDiagonal n map color (x+k) (y-k)) | k<-[0..(n-1)]])) (last (getFromListIndex [(inLeftDiagonal n map color (x+k) (y-k)) | k<-[0..(n-1)]])))]







getEdgePosColumn (Pos(fx,fy)) (Pos(lx,ly))= [Pos(fx-1,fy), Pos(lx+1,ly)]
getEdgePosRow (Pos(fx,fy)) (Pos(lx,ly))= [Pos(fx,fy-1), Pos(lx,ly+1)]
getEdgePosRDiagonal (Pos(fx,fy)) (Pos(lx,ly))= [Pos(fx-1,fy-1), Pos(lx+1,ly+1)]
getEdgePosLDiagonal (Pos(fx,fy)) (Pos(lx,ly))= [Pos(fx-1,fy+1), Pos(lx+1,ly-1)]


inColumn n map color a b 
    | and [(checkIfColor map (Pos(x, b)) color) | x<-[a..(a+n-1)]] = sort [(Pos(x, b)) | x<-[a..(a+n-1)]]
    | True = []

inRow n map color a b 
    | and [(checkIfColor map (Pos(a, y)) color) | y<-[b..(b+n-1)]] = sort [(Pos(a, y)) | y<-[b..(b+n-1)]]
    | True = []
    
inRightDiagonal n map color a b
    | and [(checkIfColor map (Pos(a+k, b+k)) color) | k<-[0..n-1]] = sort [Pos(a+k, b+k) | k<-[0..n-1]]
    | True = []

inLeftDiagonal n map color a b
    | and [(checkIfColor map (Pos(a-k, b+k)) color) | k<-[0..n-1]] = sort [Pos(a-k, b+k) | k<-[0..n-1]]
    | True = []


-}



{-










--quick check

--unlines



 
testTree :: Tree Integer
testTree = Node 1 [ Node 2 [ Node 4 [ Node 6 [], Node 8 [] ],
                            Node 5 [ Node 7 [], Node 9 [] ] ],
                   Node 3 [ Node 10 [], 
                            Node 11 [] ] 
                 ]


padding :: (Num a) => a -> String
padding 0 = ""
padding n = " " ++ padding(n-1)

showTree :: (Show a, Num b) => Tree a -> b -> String
showTree Leaf n = (padding n) ++ "."
showTree (Node a l r ) n = let showl = showTree l (n+4) in
                           let showr = showTree r (n+4) in
                           let showc = (padding n) ++ (show a) in
                           showc ++ "\n" ++ showl ++ "\n" ++ showr

instance (Show a) => Show (Tree a) where
    show a = showTree a 0



data Cell = C Color Position deriving Show

gameMap :: Integer -> Map Integer [Integer]
gameMap n = Map.fromList (map makePair [1..n])
 where makePair x = (x, [1..n])

(!) :: Position -> Color
-}
