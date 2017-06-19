import qualified Data.Map as Map
import qualified Data.Tree as Tree
import Data.List
import Data.Function (on)
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



data Color = B | W deriving Eq
newtype Position = Pos (Int, Int) deriving (Show, Eq)
newtype Board = Board(Map.Map Position Color) deriving Eq
data Game = Null | Game Board Color Position deriving (Show, Eq)--Int
data Direction = Vertical | Horizontal | DiagonalR | DiagonalL deriving Eq

-- DISPLAY FUNCTIONS --
instance Ord Position where
    compare (Pos(l1,r1)) (Pos(l2,r2))
        | l1 == l2 && r1==r2 = EQ
        | l1 == l2 && r1<r2 = LT 
        | l1 == l2 && r1>r2 = GT
        | l1 < l2 = LT
        | l1 > l2 = GT

instance Show Color where
    show B = "⚈"--"B"              
    show W = "⚆"--"W"              

mapRows = 19
cords = [1..mapRows]

instance Show (Board) where
    show x = showBoard x

alphabet = "ABCDEFGHIJKLMNOPRSTQUWXYZ"
getLetter x = alphabet!!(x-1) 

numberString = "x\\y " ++ intercalate "" (map (\x -> (whichSpace x) ++show x++" ") [1..mapRows])
letterString = "    " ++ intercalate "" (map (\x -> " "++([getLetter x])++" ") [1..mapRows])
showBoard (Board m) = ("\n"++numberString++"\n"++letterString++intercalate "" [((nextRow y x) ++ (showCell m (Pos(x, y))))| x <-cords, y<-cords])++"\n"

showCell m pos
    | Map.lookup pos m == Nothing = " - "                   --" ⊹ "
    | Map.lookup pos m == Just B = " "++ (show B) ++" "
    | Map.lookup pos m == Just W = " "++ (show W) ++" "

whichSpace x 
    | x<10 = " "
    | otherwise = ""

nextRow y x
    | y /= 1 = ""
    | y == 1 = "\n "++(show x)++ " "++(whichSpace x) 
-- DISPLAY FUNCTIONS END --

-- TREE AND GAME FUNCTIONS --
getOppositeColor c 
    | c == B = W
    | c == W = B

insertCell pos c (Board map) = Board (Map.insert pos c map)
posibleMoves (Game board color pos) = [(Game (insertCell (Pos(x,y)) (getOppositeColor color) board) (getOppositeColor color) (Pos(x,y)))| x<-cords, y<-cords, not(checkIfExists board (Pos(x,y)))]
createTree game = Tree.Node game (map createTree (posibleMoves game))
getNextTree (Tree.Node v list) pos = (filter (\x -> pos == (getNewPositionFromGame (getValueFromTree x))) list)!!0

getNewPositionFromGame (Game _ _ pos) = pos
getBoardFromGame (Game board _ _) = board
getMapFromGame (Game (Board map) _ _) = map
getColorFromGame (Game _ color _) = color
changeColorInGame (Game map color pos) = Game map (getOppositeColor color) pos
-- TREE AND GAME FUNCTIONS END--

--INITIAL--
iMap = Map.empty
iBoard = Board iMap
iGame = Game iBoard B (Pos(0,0))
iTree = createTree iGame
--END--

-- PLAYER VS CPU GAME --
main = doPvcpu
doPvcpu = pvcpu iTree

checkEnd game = ((evaluationFunction game) >= 1000)
endStr c = "\n------------------------------------------- \n" ++ "                  won: " ++ (show c) ++ " \n"++ "------------------------------------------- \n" 
showWinner game = hPutStrLn stdout $ endStr $ getColorFromGame $ game

pvcpu tree = do
        hPutStrLn stdout (show $ getBoardFromGame $ getValueFromTree $ tree)
        if(checkEnd $ getValueFromTree tree) then showWinner $ getValueFromTree tree else do        
            i <- getLine
            case parse parsePos "parse error" (i) of
                Right p -> do
                    hPutStrLn stderr $ "ruch = " ++ (show (Pos p))
                    hPutStrLn stdout $ (show $ getBoardFromGame $ getValueFromTree $ getNextTree tree (Pos p))
                    if (checkEnd $ getValueFromTree $ getNextTree tree (Pos p)) then showWinner $ getValueFromTree $ getNextTree tree (Pos p) else do
                        pvcpu $ cpuMove $ getNextTree tree (Pos p)
                Left x -> fail $ show x
-- PLAYER VS CPU GAME END --

-- CPU VS CPU GAME --
cpuvcpu tree = do
        hPutStrLn stdout (show $ getBoardFromGame $ getValueFromTree $ tree)
        if(checkEnd $ getValueFromTree tree) then showWinner $ getValueFromTree tree else do        
            hPutStrLn stdout $ (show $ getBoardFromGame $ getValueFromTree $ cpuMove $ tree)
            if (checkEnd $ getValueFromTree $ cpuMove $ tree) then showWinner $ getValueFromTree $ cpuMove $ tree else do
                cpuvcpu $ cpuMove $ cpuMove $ tree
-- CPU VS CPU GAME END--

-- PARSER --
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
-- PARSER END --

--debug cpu functions --
-- tMM1 (Tree.Node v children) =  tdoMM1 (map tMM2 children) children
-- tMM2 (Tree.Node v list) = chooseMax v (filterTreesMax list (map evaluationFunction (getValueList list)))

-- tdoMM1 l children = cbo (getListWithChangedPlayer l children) (map evaluationFunction $ getValueList l) children
-- cbo l intL parents = [((((l!!x), evaluationFunction (l!!x)), intL!!x),(chooseValues2 (intL!!x) (minimum intL) (evaluationFunction (l!!x)))) | x<-[0..(length parents)-1]] -- , (minimum intL) == (intL!!x)]

-- chooseValues1 int min r
--     | int == min = r
--     | (r>=900) && (int<1000)= r
--     | otherwise = -1200
--debug cpu functions end--

-- CPU FUNCTIONS --
cpuMove tree = minMax1 tree

minMax1 (Tree.Node v children) =  doMinMax1 (map minMax2 children) children
minMax2 (Tree.Node v list) = chooseMax v (filterTreesMax list (map evaluationFunction (getValueList list)))

attacRatio = 0

chooseValues2 int min r
    | (int == 352) && (r==160) = 600 
    | int == min = r
    | r>= int + attacRatio = r
    | otherwise = -1200

doMinMax1 l children = chooseBestOption (getListWithChangedPlayer l children) (map evaluationFunction $ getValueList l) children
getListWithChangedPlayer l parents= (zipWith (\x y-> changePlayerInGame x y) (getValueList parents) (getValueList l))
chooseBestOption l intL parents = parents !! (maxIndex ( [(chooseValues2 (intL!!x) (minimum intL) (evaluationFunction (l!!x))) | x<-[0..(length parents)-1]]))

    
chooseMax v input = input !! (maxIndex $ changeAndEvaluate v $ map getValueFromTree input)
changeAndEvaluate v l = (map evaluationFunction $ map (\x-> changePlayerInGame v x) l)
changePlayerInGame (Game pBoard pColor pPos) (Game board color pos)= Game board pColor pPos


filterTreesMax list values = filterBy (maximum values) list values
filterBy _ [] _ = []
filterBy val (x:xs) (f:xf)
    | val == f = x:(filterBy val xs xf)
    | otherwise = filterBy val xs xf

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
-- CPU FUNCTIONS END --


    -- DEPRECATED CPU FUNCTIONS --
-- dMinMax1 (Tree.Node _ list) = (list) !! (minIndex $ map evaluationFunction $ (map dMinMax2 list))
-- dMinMax2 (Tree.Node v list) = dChooseMax v $ getValueList $ filterTreesMax list (map evaluationFunction (getValueList list))
-- dChooseMax v input = input !! (maxIndex ( (changeAndEvaluate v input)))
    -- DEPRECATED CPU FUNCTIONS END--

--debug cpu logic -- 
-- getBestDirection fList = fList !! (maxIndex $ map calculateProfitability fList)
-- foo = getBestDirection $ functionList2 testGame5
-- foo3 = map calculateProfitability $ functionList2 testGame5
-- foo game =  cp $ functionList game
-- gvom  a b c d = putStrLn $ (show a)++" "++(show b)++" "++(show c)++" "++(show d)
-- cp [(pV:oV:eV:_),(pH:oH:eH:_),(pDR:oDR:eDR:_),(pDL:oDL:eDL:_)] = gvom ((length pV), (length oV), (length eV)) ((length pH), (length oH), (length eH)) ((length pDR), (length oDR), (length eDR)) ((length pDL), (length oDL), (length eDL))
--debug cpu logic end--

-- CPU LOGIC --
evaluationFunction game = calculateProfitability $ functionList game

directionList = [Vertical, Horizontal, DiagonalR, DiagonalL]
functionList game = map (\d -> checkInArea d game) directionList

calculateProfitability [(pV:oV:eV:_),(pH:oH:eH:_),(pDR:oDR:eDR:_),(pDL:oDL:eDL:_)] = getValueOfMap ((length pV), (length oV), (length eV)) ((length pH), (length oH), (length eH)) ((length pDR), (length oDR), (length eDR)) ((length pDL), (length oDL), (length eDL))

getValueOfMap (pV,oV,eV) (pH,oH,eH) (pDR,oDR,eDR) (pDL,oDL,eDL)
    | or $ map fiveInRow [pV,pH,pDR,pDL] = 1000 + blockBonus oV oH oDR oDL
    | or $ (zipWith (&&) (map fourInRow [pV,pH,pDR,pDL]) (map (==2) [eV,eH,eDR,eDL]))  = 900 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL
    | or $ (zipWith (&&) (map fourInRow [pV,pH,pDR,pDL]) (map (==1) [eV,eH,eDR,eDL]))  = 800 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL 
    | or $ (zipWith (&&) (map threeInRow [pV,pH,pDR,pDL]) (map (==2) [eV,eH,eDR,eDL]))  = 600 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL
    | or $ (zipWith (&&) (map threeInRow [pV,pH,pDR,pDL]) (map (==1) [eV,eH,eDR,eDL]))  = 400 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL
    | or $ (zipWith (&&) (map twoInRow [pV,pH,pDR,pDL]) (map (==2) [eV,eH,eDR,eDL]))  = 328 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL
    | or $ (zipWith (&&) (map twoInRow [pV,pH,pDR,pDL]) (map (==1) [eV,eH,eDR,eDL]))  = 300 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL
    | or $ (zipWith (&&) (map oneInRow [pV,pH,pDR,pDL]) (map (==2) [eV,eH,eDR,eDL]))  = 128 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL
    | or $ (zipWith (&&) (map oneInRow [pV,pH,pDR,pDL]) (map (==1) [eV,eH,eDR,eDL]))  = 100 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL 
    | or $ (zipWith (&&) (map oneInRow [pV,pH,pDR,pDL]) (map (==0) [eV,eH,eDR,eDL]))  = -30 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL 
    | or $ (zipWith (&&) (map twoInRow [pV,pH,pDR,pDL]) (map (==0) [eV,eH,eDR,eDL]))  = -30 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL
    | or $ (zipWith (&&) (map threeInRow [pV,pH,pDR,pDL]) (map (==0) [eV,eH,eDR,eDL]))  = -30 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL
    | or $ (zipWith (&&) (map fourInRow [pV,pH,pDR,pDL]) (map (==0) [eV,eH,eDR,eDL]))  = -30 + blockBonus oV oH oDR oDL + emptyBonus eV eH eDR eDL
    | otherwise = 0

fiveInRow p = p >= 5
fourInRow p = p == 4
threeInRow p = p == 3
twoInRow p = p == 2
oneInRow p = p == 1

blockBonus o1 o2 o3 o4= o1*10+o2*10+o3*10+o4*10
emptyBonus e1 e2 e3 e4= e1*2+e2*2+e3*2+e4*2

checkInArea direction game =getPOEArray (getPlayerPositions direction game) direction game 

getPOEArray playerPos direction game 
    | length playerPos > 0 = [playerPos, getO game direction playerPos, getE game direction playerPos] 
    | otherwise = [[],[],[]]

getO game direction playerPos = getOpponentPosition game $ getEdgePosition direction playerPos
getE game direction playerPos = getEmptyPosition game $ getEdgePosition direction playerPos

mostCommonElem list = fst $ maximumBy (compare `on` snd) elemCounts where
    elemCounts = nub [(element, count) | element <- list, let count = length (filter (==element) list)]

getXFromPos (Pos(x,y)) = x
getYFromPos (Pos(x,y)) = y

getPosValueFromList direction list
    | (direction == Vertical) = map getXFromPos list
    | (direction == Horizontal) = map getYFromPos list
    | (direction == DiagonalR) = map getYFromPos list
    | (direction == DiagonalL) = map getYFromPos list

template = reverse [0..10] 
createIntTable direction list = zipWith (+) template (getPosValueFromList direction list)

getInARow direction list = filter (\x -> (x/=(Pos(0,0)))) $ zipWith (\x y -> ( if ((mostCommonElem $ createIntTable direction list)==x) then (y) else (Pos(0,0))))  (createIntTable direction list) list

getPlayerPositions direction (Game (Board map) color (Pos(x,y)))
    | (direction == Vertical) = getInARow direction $ filter (\p -> (checkIfColor map p color)) [(Pos(a,y)) | a<-[x-5..x+5]]
    | (direction == Horizontal) = getInARow direction $ filter (\p -> (checkIfColor map p color)) [(Pos(x,b)) | b<-[y-5..y+5]]
    | (direction == DiagonalR) = getInARow direction $ filter (\p -> (checkIfColor map p color)) [(Pos(x+k,y+k)) | k<-[-5..5]]
    | (direction == DiagonalL) = getInARow direction $ filter (\p -> (checkIfColor map p color)) [(Pos(x-k,y+k)) | k<-[-5..5]]

getPrev direction (Pos(x,y))
    | (direction == Vertical) = Pos(x-1,y)
    | (direction == Horizontal) = Pos(x,y-1)
    | (direction == DiagonalR) = Pos(x-1,y-1)
    | (direction == DiagonalL) = Pos(x+1,y-1)
    
getNext direction (Pos(x,y)) 
    | (direction == Vertical) = Pos(x+1,y)
    | (direction == Horizontal) = Pos(x,y+1)
    | (direction == DiagonalR) = Pos(x+1,y+1)
    | (direction == DiagonalL) = Pos(x-1,y+1)

getOpponentPosition (Game (Board map) color _) (pPos, nPos) = filter (\p -> (checkIfColor map p (getOppositeColor color))) [pPos, nPos]
getEmptyPosition (Game (Board map) _ _) (pPos, nPos) = filter (\p -> (isOnBoard p && (Map.lookup p map == Nothing))) [pPos, nPos]
getEdgePosition direction l = (getPrev direction $ head l, getNext direction $ last l)

isOnBoard (Pos(x,y)) = x > 0 && x <= mapRows && y > 0 && y <= mapRows 
checkIfColor map pos color = (Map.lookup pos map == Just color) && (isOnBoard pos)
checkIfExists (Board map) pos = isOnBoard pos && (Map.lookup pos map /= Nothing)  
-- CPU LOGIC END --


--TESTS--
-- testMap1 = Map.fromList ([(Pos(x,y), W)| x <-[8..13], y<-[4..7]]++[(Pos(12,8), W),(Pos(12,3), W)]++[(Pos(7,4), B)])
-- testMap2 = Map.fromList ([(Pos(5+k,6+k), B)| k <-[0..3]]++[(Pos(5+k,4+k), B)| k <-[0..3]]++[(Pos(5+k,5+k), W)| k <-[0..4]])

-- testMap3 = Map.fromList ([(Pos(1,4), W),(Pos(1,3), W),(Pos(1,2), W),(Pos(2,4), W),(Pos(2,3), B)])
-- testBoard3 = Board(testMap3)
-- testGame3 = Game testBoard3 B (Pos(2,3))
-- testTree3 = createTree testGame3

-- testMap4 = Map.fromList ([(Pos(1,1), B)])
-- testBoard4 = Board(testMap4)
-- testGame4 = Game testBoard4 B (Pos(1,1))

-- testMap6 = Map.fromList ([(Pos(1,2), W)])
-- testBoard6 = Board(testMap6)
-- testGame6 = Game testBoard6 W (Pos(1,2))

-- testMap7 = Map.fromList ([(Pos(1,3), B)])
-- testBoard7 = Board(testMap7)
-- testGame7 = Game testBoard7 B (Pos(1,3))

-- testMap5 = Map.fromList ([(Pos(16+k,6), B)| k <-[0..3]]++[(Pos(15,6), W)])
-- testBoard5 = Board(testMap5)
-- testGame5 = Game testBoard5 B (Pos(16,6))

-- testMap8 = Map.fromList ([(Pos(6,6), W)])
-- testBoard8 = Board(testMap8)
-- testGame8 = Game testBoard8 W (Pos(6,6))
-- testTree8 = createTree testGame8

-- testMap9 = Map.fromList ([(Pos(1,1), W)])
-- testBoard9 = Board(testMap9)
-- testGame9 = Game testBoard9 W (Pos(1,1))
-- testTree9 = createTree testGame9

-- testMap10 = Map.fromList ([(Pos (5,1),W),(Pos (5,2),B),(Pos (5,3),B),(Pos (5,4),B),(Pos (5,5),B),(Pos (4,3),W),(Pos (4,4),W),(Pos (4,5),W)])
-- testBoard10 = Board(testMap10)
-- testGame10 = Game testBoard10 B (Pos(5,5))
-- testTree10 = createTree testGame10

-- testMap11 = Map.fromList ([(Pos(8,3),W),(Pos (8,4),B),(Pos (9,3),B),(Pos (9,4),W),(Pos (9,5),B),(Pos (10,2),B),(Pos (10,4),W),(Pos (11,2),B),(Pos (11,3),W),(Pos (11,4),W),(Pos (11,5),W)])
-- testBoard11 = Board(testMap11)
-- testGame11 = Game testBoard11 W (Pos(8,3))
-- testTree11 = createTree testGame11

-- testMap12 = Map.fromList ([(Pos (3,1),B),(Pos (3,4),B),(Pos (3,5),B),(Pos (4,5),W),(Pos (4,6),W),(Pos (4,7),W)])
-- testBoard12 = Board(testMap12)
-- testGame12 = Game testBoard12 B (Pos(3,1))
-- testTree12 = createTree testGame12

-- testMap13 = Map.fromList ([(Pos (4,4),B),(Pos (3,4),B),(Pos (3,5),B),(Pos (4,5),W),(Pos (4,6),W),(Pos (4,7),W)])
-- testBoard13 = Board(testMap13)
-- testGame13 = Game testBoard13 B (Pos(4,4))
-- testTree13 = createTree testGame13

-- testMap14 = Map.fromList ([(Pos (3,3),B),(Pos (3,2),B), (Pos (3,1),B),(Pos (3,4),B),(Pos (4,4),W),(Pos (5,5),W),(Pos (6,6),W)])
-- testBoard14 = Board(testMap14)
-- testGame14 = Game testBoard14 B (Pos(3,3))
-- testTree14 = createTree testGame14

-- testMap15 = Map.fromList ([(Pos (4,5),B),(Pos (4,4),B),(Pos (4,6),B),(Pos (5,5),W),(Pos (5,6),W)])
-- testBoard15 = Board(testMap15)
-- testGame15 = Game testBoard15 W (Pos(5,6))
-- testTree15 = createTree testGame15

-- testMap16 = Map.fromList ([(Pos (10,10),B),(Pos (11,11),W)])
-- testBoard16 = Board(testMap16)
-- testGame16 = Game testBoard16 B (Pos(4,4))
-- testTree16 = createTree testGame16

-- testMap17 = Map.fromList [(Pos (4,5),B),(Pos (4,6),W),(Pos (5,5),W)]
-- testBoard17 = Board(testMap17)
-- testGame17 = Game testBoard17 W (Pos(4,6))
-- testTree17 = createTree testGame17

-- testMap18 = Map.fromList [(Pos (4,4),B),(Pos (4,5),W),(Pos (5,5),W)]
-- testBoard18 = Board(testMap18)
-- testGame18 = Game testBoard18 W (Pos(4,5))
-- testTree18 = createTree testGame18

-- sMap1 = Map.fromList ([(Pos (5,5),W)])
-- sBoard1 = Board(sMap1)
-- sGame1 = Game sBoard1 W (Pos(5,5))

-- sMap2a = Map.fromList ([(Pos (5,5),W), (Pos(4, 5), B)])
-- sBoard2a = Board(sMap2a)
-- sGame2a = Game sBoard2a B (Pos(4,5))

-- sMap2b = Map.fromList ([(Pos (5,5),W), (Pos(1, 1), B)])
-- sBoard2b = Board(sMap2b)
-- sGame2b = Game sBoard2b B (Pos(1,1))

-- sMap2c = Map.fromList ([(Pos (5,5),W), (Pos(3, 4), B)])
-- sBoard2c = Board(sMap2c)
-- sGame2c = Game sBoard2c B (Pos(3,4))

-- sTree = Tree.Node sGame1 [((createTree sGame2a)),((createTree sGame2b)),((createTree sGame2c)) ]
--END--