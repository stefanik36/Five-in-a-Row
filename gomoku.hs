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
    show B = "⚈"--"B"                --"⚈"
    show W = "⚆"--"W"                --"⚆"

mapRows = 10
cords = [1..mapRows]

instance Show (Board) where
    show x = showBoard x

numberString = "x\\y " ++ intercalate "" (map (\x -> (whichSpace x) ++show x++" ") [1..mapRows])
showBoard (Board m) = ("\n"++numberString++intercalate "" [((nextRow y x) ++ (showCell m (Pos(x, y))))| x <-cords, y<-cords])++"\n"

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


testMap8 = Map.fromList ([(Pos(6,6), W)])
testBoard8 = Board(testMap8)
testGame8 = Game testBoard8 W (Pos(6,6))
testTree8 = createTree testGame8

testMap9 = Map.fromList ([(Pos(1,1), W)])
testBoard9 = Board(testMap9)
testGame9 = Game testBoard9 W (Pos(1,1))
testTree9 = createTree testGame9


testMap10 = Map.fromList ([(Pos (1,1),W),(Pos (1,2),W),(Pos (1,3),W),(Pos (1,5),B),(Pos (2,2),W),(Pos (3,1),W),(Pos (3,2),W),(Pos (3,3),W),(Pos (3,5),B),(Pos (5,1),B),(Pos (5,2),B),(Pos (5,3),B),(Pos (5,4),B),(Pos (5,5),B)])
testBoard10 = Board(testMap10)
testGame10 = Game testBoard10 B (Pos(5,1))
testTree10 = createTree testGame10


testMap11 = Map.fromList ([(Pos(6,6), W)])
testBoard11 = Board(testMap11)
testGame11 = Game testBoard11 W (Pos(6,6))
testTree11 = createTree testGame11

testMap12 = Map.fromList ([(Pos (3,1),B),(Pos (3,4),B),(Pos (3,5),B),(Pos (4,5),W),(Pos (4,6),W),(Pos (4,7),W)])
testBoard12 = Board(testMap12)
testGame12 = Game testBoard12 B (Pos(3,1))
testTree12 = createTree testGame12


testMap13 = Map.fromList ([(Pos (4,4),B),(Pos (3,4),B),(Pos (3,5),B),(Pos (4,5),W),(Pos (4,6),W),(Pos (4,7),W)])
testBoard13 = Board(testMap13)
testGame13 = Game testBoard13 B (Pos(4,4))
testTree13 = createTree testGame13

testMap14 = Map.fromList ([(Pos (3,3),B),(Pos (3,2),B), (Pos (3,1),B),(Pos (3,4),B),(Pos (4,4),W),(Pos (5,5),W),(Pos (6,6),W)])
testBoard14 = Board(testMap14)
testGame14 = Game testBoard14 B (Pos(3,3))
testTree14 = createTree testGame14

testMap15 = Map.fromList ([(Pos (4,5),B),(Pos (4,4),B),(Pos (5,5),W),(Pos (5,6),W)])
testBoard15 = Board(testMap15)
testGame15 = Game testBoard15 B (Pos(4,5))
testTree15 = createTree testGame15

sMap1 = Map.fromList ([(Pos (5,5),W)])
sBoard1 = Board(sMap1)
sGame1 = Game sBoard1 W (Pos(5,5))

sMap2a = Map.fromList ([(Pos (5,5),W), (Pos(4, 5), B)])
sBoard2a = Board(sMap2a)
sGame2a = Game sBoard2a B (Pos(4,5))


sMap2b = Map.fromList ([(Pos (5,5),W), (Pos(1, 1), B)])
sBoard2b = Board(sMap2b)
sGame2b = Game sBoard2b B (Pos(1,1))

sMap2c = Map.fromList ([(Pos (5,5),W), (Pos(3, 4), B)])
sBoard2c = Board(sMap2c)
sGame2c = Game sBoard2c B (Pos(3,4))

sTree = Tree.Node sGame1 [((createTree sGame2a)),((createTree sGame2b)),((createTree sGame2c)) ]


testMap16 = Map.fromList ([(Pos (4,4),B),(Pos (5,5),W)])
testBoard16 = Board(testMap16)
testGame16 = Game testBoard16 B (Pos(4,4))
testTree16 = createTree testGame16


testMap17 = Map.fromList [(Pos (4,5),B),(Pos (4,6),W),(Pos (5,5),W)]
testBoard17 = Board(testMap17)
testGame17 = Game testBoard17 W (Pos(4,6))
testTree17 = createTree testGame17

testMap18 = Map.fromList [(Pos (4,4),B),(Pos (4,5),W),(Pos (5,5),W)]
testBoard18 = Board(testMap18)
testGame18 = Game testBoard18 W (Pos(4,5))
testTree18 = createTree testGame18
--END--



-- PLAYER VS CPU GAME --
main = doPvcpu
doPvcpu = pvcpu iTree

checkEnd game = ((evaluationFunction game) >= 1000)
showWinner game = hPutStrLn stdout $ "won: " ++ (show $ getColorFromGame game)

pvcpu tree = do
        hPutStrLn stdout $ (show $ getBoardFromGame $ getValueFromTree $ tree) --  ++ "\n map: "++(show $ getMapFromGame $ getValueFromTree $ tree)
        if(checkEnd $ getValueFromTree tree) then showWinner $ getValueFromTree tree else do        
            i <- getLine
            case parse parsePos "parse error" (i) of
                Right p -> do
                    hPutStrLn stderr $ "ruch = " ++ (show (Pos p))
                    hPutStrLn stdout $ (show $ getBoardFromGame $ getValueFromTree $ getNextTree tree (Pos p)) -- ++ "\n map: "++(show $ getMapFromGame $ getValueFromTree $ getNextTree tree (Pos p))
                    if (checkEnd $ getValueFromTree $ getNextTree tree (Pos p)) then showWinner $ getValueFromTree $ getNextTree tree (Pos p) else do
                        pvcpu $ cpuMove $ getNextTree tree (Pos p)
                Left x -> fail $ show x
-- PLAYER VS CPU GAME END--

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

-- CPU FUNCTIONS --
cpuMove tree = gcm5 tree


minMax1 (Tree.Node _ list) = list !! maxIndex (map minMax2 list)--map minMax2 list)--(map getValueFromTree list) !! maxIndex (map gcm2 list)
minMax2 (Tree.Node v list) = evaluationFunction $ changeGame1 v ((getValueList $ filterTreesMax list (map evaluationFunction (getValueList list)))!!0) -- (map minMax3 list) --gcm3 list) --(map gcm3 list)
-- minMax3 (Tree.Node _ list) = maximum (map evaluationFunction (getValueList list))

gcm1 (Tree.Node v list) =  map evaluationFunction $ map modified1 list
 
gcm4 (Tree.Node v list) = map evaluationFunction (map modified1 list)


newFoo2 l = filterTreesMin (l) (map evaluationFunction l)







-- nn [] = [] 
-- nn (x:xs) = chooseMax4 (getValueFromTree x) (ftm l)

ftm l = filterTreesMin l $ map evaluationFunction $ getValueList l


getIndexFromJust (Just index) = index

newFoo3 l r v = [(r!!(getIndexFromJust (elemIndex (v!!x) l) )) | x<-[0..(length v)-1] ] 


-- newFoo5 l r = newFoo3 l r (newFoo4 l)

getTreeChildren (Tree.Node _ list) = list

ffff l = getValueList $ filterTreesMax l $ map evaluationFunction $getValueList l

gcm5 (Tree.Node v children) =  newFoo6 (map modified2 children) children --head $ newFoo5 (map modified2 list) list -- =ffff (map modified2 list) ----head $ filter (\x -> (x == (head $ newFoo3 $ (map modified2 list)))) list--list !! (maxIndex $ map evaluationFunction $ map (\x ->changeGame1 (getValueFromTree x) (modified1 x)) list)) -- map (\x-> changeGame1 v x) (newFoo2 (map modified1 list))

newFoo6 l children = newFoo4 (newFoo5 l children) ( map evaluationFunction $ getValueList l) children

newFoo5 l parents= (zipWith (\x y-> changeGame1 x y) (getValueList parents) (getValueList l))


newFoo4 l intL parents =parents !! (maxIndex ( [(newFoo7 (intL!!x) (minimum intL) (evaluationFunction (l!!x))) | x<-[0..(length parents)-1]]))-- parents !! (maxIndex ( [(newFoo7 (intL!!x) (minimum intL) (evaluationFunction (l!!x))) | x<-[0..(length parents)-1]])) 

newFoo7 int min r
    | int == min = r
    | r>=800 = r
    | otherwise = -1200



newFoo v l 
    | length (filter (\x-> (head (map evaluationFunction l))==x) (map evaluationFunction l)) == length l = maxIndex $ changeAndEvaluate v l
    | otherwise = minIndex $ map evaluationFunction l



gcm2 (Tree.Node _ list) =  map evaluationFunction $ getValueList $ filterTreesMax list (map evaluationFunction (getValueList list))--(map gcm3 list)-- (map getChildrenMax3 list)--(map evaluationFunction (getValueList list))--(map getChildrenMax3 list)
gcm3 (Tree.Node _ list) = (list) !! (minIndex $ map evaluationFunction $ (map modified1 list))--(map modified2 list) -- getValueFromTree $ list !! minIndex (map modified2 list)-- getValueList list--maximum (map evaluationFunction (getValueList list))


filterBy _ [] _ = []
filterBy val (x:xs) (f:xf)
    | val == f = x:(filterBy val xs xf)
    | otherwise = filterBy val xs xf

changeGame1 (Game pBoard pColor pPos) (Game board color pos)= Game board pColor pPos


filterTreesMax list values = filterBy (maximum values) list values
filterTreesMin list values = filterBy (minimum values) list values

chooseMax v input = input !! (maxIndex ( (changeAndEvaluate v input)))
chooseMin v input = input !! (minIndex ( (changeAndEvaluate v input)))

changeAndEvaluate v l = (map evaluationFunction $ map (\x-> changeGame1 v x) l)
chooseMax2 [] = []
chooseMax2 (x:xs) = (maximum x):(chooseMax2 xs)
chooseMax3 v input r = r !! (maxIndex $ changeAndEvaluate v $ map getValueFromTree input)
chooseMin3 v input r = r !! (minIndex $ changeAndEvaluate v $ map getValueFromTree input)
chooseMax4 v input = input !! (maxIndex $ changeAndEvaluate v $ map getValueFromTree input)
chooseMin4 v input = input !! (minIndex $ changeAndEvaluate v $ map getValueFromTree input)

chooseMaxLL v input = input !! (maxIndex $ zipWith (\x y -> changeAndEvaluate x $ getValueFromTree y) (getValueList v) input)


-- modified3 (Tree.Node v list) = evaluationFunction $ ((getValueList $ filterTreesMax list (map evaluationFunction (getValueList list)))!!0)
modified3 (Tree.Node v list) = evaluationFunction $ chooseMax v $ getValueList $ filterTreesMax list (map evaluationFunction (getValueList list))
modified1 (Tree.Node v list) = chooseMax v $ getValueList $ filterTreesMax list (map evaluationFunction (getValueList list)) --map evaluationFunction $ map (\x-> changeGame1 v x) (getValueList $ filterTreesMax list (map evaluationFunction (getValueList list)))  -- map (\x-> changeGame1 v x) (getValueList $ filterTreesMax list (map evaluationFunction (getValueList list)))


modified2 (Tree.Node v list) = chooseMax4 v (filterTreesMax list (map evaluationFunction (getValueList list))) --map evaluationFunction $ map (\x-> changeGame1 v x) (getValueList $ filterTreesMax list (map evaluationFunction (getValueList list)))  -- map (\x-> changeGame1 v x) (getValueList $ filterTreesMax list (map evaluationFunction (getValueList list)))






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
-- CPU FUNCTIONS END--



--to test-- 
-- getBestDirection fList = fList !! (maxIndex $ map calculateProfitability fList)
-- foo = getBestDirection $ functionList2 testGame5
-- foo3 = map calculateProfitability $ functionList2 testGame5
--to test end--




-- CPU LOGIC --
-- calculateProfitability (player:opponent:empty) = (length player)*20 + (calculateEmptysProfitability (head empty))





fiveInRow p = p >= 5
fourInRow p = p == 4
threeInRow p = p == 3
twoInRow p = p == 2
oneInRow p = p == 1

blockBonus o1 o2 o3 o4= o1*10+o2*10+o3*10+o4*10



getValueOfMap (pV,oV,eV) (pH,oH,eH) (pDR,oDR,eDR) (pDL,oDL,eDL)
    | or $ map fiveInRow [pV,pH,pDR,pDL] = 1000 + blockBonus oV oH oDR oDL
    | or $ (zipWith (&&) (map fourInRow [pV,pH,pDR,pDL]) (map (==2) [eV,eH,eDR,eDL]))  = 900 + blockBonus oV oH oDR oDL
    | or $ (zipWith (&&) (map fourInRow [pV,pH,pDR,pDL]) (map (==1) [eV,eH,eDR,eDL]))  = 800 + blockBonus oV oH oDR oDL 
    | or $ (zipWith (&&) (map threeInRow [pV,pH,pDR,pDL]) (map (==2) [eV,eH,eDR,eDL]))  = 600 + blockBonus oV oH oDR oDL
    | or $ (zipWith (&&) (map threeInRow [pV,pH,pDR,pDL]) (map (==1) [eV,eH,eDR,eDL]))  = 400 + blockBonus oV oH oDR oDL
    | or $ (zipWith (&&) (map twoInRow [pV,pH,pDR,pDL]) (map (==2) [eV,eH,eDR,eDL]))  = 328 + blockBonus oV oH oDR oDL
    | or $ (zipWith (&&) (map twoInRow [pV,pH,pDR,pDL]) (map (==1) [eV,eH,eDR,eDL]))  = 300 + blockBonus oV oH oDR oDL
    | or $ (zipWith (&&) (map oneInRow [pV,pH,pDR,pDL]) (map (==2) [eV,eH,eDR,eDL]))  = 128 + blockBonus oV oH oDR oDL
    | or $ (zipWith (&&) (map oneInRow [pV,pH,pDR,pDL]) (map (==1) [eV,eH,eDR,eDL]))  = 100 + blockBonus oV oH oDR oDL 
    | or $ (zipWith (&&) (map oneInRow [pV,pH,pDR,pDL]) (map (==0) [eV,eH,eDR,eDL]))  = -30 + blockBonus oV oH oDR oDL 
    | or $ (zipWith (&&) (map twoInRow [pV,pH,pDR,pDL]) (map (==0) [eV,eH,eDR,eDL]))  = -30 + blockBonus oV oH oDR oDL
    | or $ (zipWith (&&) (map threeInRow [pV,pH,pDR,pDL]) (map (==0) [eV,eH,eDR,eDL]))  = -30 + blockBonus oV oH oDR oDL
    | or $ (zipWith (&&) (map fourInRow [pV,pH,pDR,pDL]) (map (==0) [eV,eH,eDR,eDL]))  = -30 + blockBonus oV oH oDR oDL
    | otherwise = 0

-- blockBonus o1 o2 o3 = o1*5+o2*5+o3*5

    -- | fiveInRow pV = 1000 + blockBonus oH oDR oDL
    -- | fiveInRow pH = 1000 + blockBonus oV oDR oDL
    -- | fiveInRow pDR = 1000 + blockBonus oH oV oDL
    -- | fiveInRow pDL = 1000 + blockBonus oH oV oDR
    -- | (fourInRow pV) && (eV==2) = 900 + blockBonus oH oDR oDL
    -- | (fourInRow pH) && (eH==2) = 900 + blockBonus oV oDR oDL
    -- | (fourInRow pDR) && (eDR==2) = 900 + blockBonus oH oV oDL
    -- | (fourInRow pDL) && (eDL==2) = 900 + blockBonus oH oV oDR
    -- | (fourInRow pV) && (eV==1) = 600 + blockBonus oH oDR oDL
    -- | (fourInRow pH) && (eH==1) = 600 + blockBonus oV oDR oDL
    -- | (fourInRow pDR) && (eDR==1) = 600 + blockBonus oH oV oDL
    -- | (fourInRow pDL) && (eDL==1) = 600 + blockBonus oH oV oDR
    -- | (threeInRow pV) && (eV==2) = 400 + blockBonus oH oDR oDL
    -- | (threeInRow pH) && (eH==2) = 400 + blockBonus oV oDR oDL
    -- | (threeInRow pDR) && (eDR==2) = 400 + blockBonus oH oV oDL
    -- | (threeInRow pDL) && (eDL==2) = 400 + blockBonus oH oV oDR
    -- | (threeInRow pV) && (eV==1) = 350 + blockBonus oH oDR oDL
    -- | (threeInRow pH) && (eH==1) = 350 + blockBonus oV oDR oDL
    -- | (threeInRow pDR) && (eDR==1) = 350 + blockBonus oH oV oDL
    -- | (threeInRow pDL) && (eDL==1) = 350 + blockBonus oH oV oDR
    -- | (twoInRow pV) && (eV==2) = 350 + blockBonus oH oDR oDL
    -- | (twoInRow pH) && (eH==2) = 350 + blockBonus oV oDR oDL
    -- | (twoInRow pDR) && (eDR==2) = 350 + blockBonus oH oV oDL
    -- | (twoInRow pDL) && (eDL==2) = 350 + blockBonus oH oV oDR
    -- | (twoInRow pV) && (eV==1) = 200 + blockBonus oH oDR oDL
    -- | (twoInRow pH) && (eH==1) = 200 + blockBonus oV oDR oDL
    -- | (twoInRow pDR) && (eDR==1) = 200 + blockBonus oH oV oDL
    -- | (twoInRow pDL) && (eDL==1) = 200 + blockBonus oH oV oDR
    -- | (oneInRow pV) && (eV==2) = 180 + blockBonus oH oDR oDL
    -- | (oneInRow pH) && (eH==2) = 180 + blockBonus oV oDR oDL
    -- | (oneInRow pDR) && (eDR==2) = 180 + blockBonus oH oV oDL
    -- | (oneInRow pDL) && (eDL==2) = 180 + blockBonus oH oV oDR
    -- | (oneInRow pV) && (eV==1) = 100 + blockBonus oH oDR oDL
    -- | (oneInRow pH) && (eH==1) = 100 + blockBonus oV oDR oDL
    -- | (oneInRow pDR) && (eDR==1) = 100 + blockBonus oH oV oDL
    -- | (oneInRow pDL) && (eDL==1) = 100 + blockBonus oH oV oDR
    -- | otherwise = 0

gvom  a b c d = putStrLn $ (show a)++" "++(show b)++" "++(show c)++" "++(show d)
cp [(pV:oV:eV:_),(pH:oH:eH:_),(pDR:oDR:eDR:_),(pDL:oDL:eDL:_)] = gvom ((length pV), (length oV), (length eV)) ((length pH), (length oH), (length eH)) ((length pDR), (length oDR), (length eDR)) ((length pDL), (length oDL), (length eDL))


    -- | or $ map fiveInRow [pV,pH,pDR,pDL] = 1000
    -- | or $ (zipWith (&&) (map fourInRow [pV,pH,pDR,pDL]) (map (\x -> x>0) [eV,eH,eDR,eDL]))  = 900 
    -- | or $ (zipWith (&&) (map threeInRow [pV,pH,pDR,pDL]) (map (\x -> x>1) [eV,eH,eDR,eDL]))  = 600
    -- | or $ (zipWith (&&) (map threeInRow [pV,pH,pDR,pDL]) (map (\x -> x>0) [eV,eH,eDR,eDL]))  = 400
    -- | or $ (zipWith (&&) (map twoInRow [pV,pH,pDR,pDL]) (map (\x -> x>0) [eV,eH,eDR,eDL]))  = 350
    -- | or $ (zipWith (&&) (map twoInRow [pV,pH,pDR,pDL]) (map (\x -> x>1) [eV,eH,eDR,eDL]))  = 200
    -- | or $ (zipWith (&&) (map oneInRow [pV,pH,pDR,pDL]) (map (\x -> x>0) [eV,eH,eDR,eDL]))  = 180
    -- | or $ (zipWith (&&) (map oneInRow [pV,pH,pDR,pDL]) (map (\x -> x>1) [eV,eH,eDR,eDL]))  = 100 
    -- | otherwise = 0


calculateProfitability [(pV:oV:eV:_),(pH:oH:eH:_),(pDR:oDR:eDR:_),(pDL:oDL:eDL:_)] = getValueOfMap ((length pV), (length oV), (length eV)) ((length pH), (length oH), (length eH)) ((length pDR), (length oDR), (length eDR)) ((length pDL), (length oDL), (length eDL))



calculateEmptysProfitability emptys = sum $ zipWith (*) [-62, 101,-31] (map ((length emptys)^) [0..2])

foo game =  cp $ functionList game

evaluationFunction game = calculateProfitability $ functionList game


directionList = [Vertical, Horizontal, DiagonalR, DiagonalL]
functionList game = map (\d -> checkInArea d game) directionList



checkInArea direction game =getPOEArray (getPlayerPositions direction game) direction game 

getPOEArray playerPos direction game 
    | length playerPos > 0 = [playerPos, getO game direction playerPos, getE game direction playerPos] 
    | otherwise = [[],[],[]]

getO game direction playerPos = getOpponentPosition game $ getEdgePosition direction playerPos
getE game direction playerPos = getEmptyPosition game $ getEdgePosition direction playerPos


template = reverse [0..10] 

mostCommonElem list = fst $ maximumBy (compare `on` snd) elemCounts where
    elemCounts = nub [(element, count) | element <- list, let count = length (filter (==element) list)]

getXFromPos (Pos(x,y)) = x
getYFromPos (Pos(x,y)) = y

getPosValueFromList direction list
    | (direction == Vertical) = map getXFromPos list
    | (direction == Horizontal) = map getYFromPos list
    | (direction == DiagonalR) = map getYFromPos list
    | (direction == DiagonalL) = map getYFromPos list

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