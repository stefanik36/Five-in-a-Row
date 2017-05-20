import qualified Data.Map as Map
import qualified Data.Tree as Tree
import Data.List

data Color = B | W deriving Eq
newtype Position = Pos (Int, Int) deriving (Show, Eq)
newtype Board = Board(Map.Map Position Color)

instance Ord Position where
    compare (Pos(l1,r1)) (Pos(l2,r2))
        | l1 == l2 && r1==r2 = EQ
        | l1 == l2 && r1<r2 = LT 
        | l1 == l2 && r1>r2 = GT
        | l1 < l2 = LT
        | l1 > l2 = GT

instance Show Color where
    show B = "x"
    show W = "o"

mapRows = 19
cords = [1..mapRows]

bMap = Map.fromList []

bBoard = Board bMap


instance Show (Board) where
    show x = showBoard x


numberString = "x\\y  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 "
showBoard :: (Board) -> [Char]
showBoard (Board m) = "\n"++numberString++intercalate "" [((nextRow y x) ++ (showCell m (Pos(x, y))))| x <-cords, y<-cords]

showCell m pos
    | Map.lookup pos m == Nothing = " - "
    | Map.lookup pos m == Just B = " "++ (show B) ++" "
    | Map.lookup pos m == Just W = " "++ (show W) ++" "


nextRow y x
    | y /= 1 = ""
    | y == 1 && x<10 = "\n "++(show x)++"  "
    | y == 1 && x>=10 = "\n "++(show x)++" "
getMap (Board(m)) = m

insertCell pos c (Board map) = Board (Map.insert pos c map)


-- #lab4


--Game -> [Game]
--f: Board->C -> [Board]

checkIfExists (Board map) pos = Map.lookup pos map /= Nothing

getOppositeColor c 
    | c == B = W
    | c == W = B



data Game = Game Board Color Position deriving Show--Int

bGame = Game bBoard B (Pos(0,0))




posibleMoves (Game board color pos) =  [(Game (insertCell (Pos(x,y)) color board) (getOppositeColor color) (Pos(x,y)))| x<-cords, y<-cords, not(checkIfExists board (Pos(x,y)))]


--createTree (Game (Board m) color)
--    | m == Map.fromList [] = []


createTree (Game board color pos) = Tree.Node (Game board color pos) (map createTree (posibleMoves (Game board color pos)))




evaluationFunction :: Game -> Int

evaluationFunction game
    | isOption1 game = 10
    | isOption2 game = 9

--5 in line 
isOption1 (Game board color newPos)
    | inLine 5 0 board color newPos /= [] = True
    | True = False

isOption2 (Game board color newPos) 
    | inLine 4 1 board color newPos /= [] = True
    | True = False





inLine n emptys (Board map) color pos -- = True
   -- | checkColumnInArea n emptys map color pos /= [] = checkColumnInArea n emptys map color pos
    | checkRowInArea n emptys map color pos /= [] = checkRowInArea n emptys map color pos
    | checkRightDiagonalInArea n emptys map color pos /= [] = checkRightDiagonalInArea n emptys map color pos 
    | checkLeftDiagonalInArea n emptys map color pos /= [] = checkLeftDiagonalInArea n emptys map color pos
    | True = []





checkColumnInArea n emptys map color (Pos(x,y)) = [getFromListIndex [(inColumn n map color a y) | a<-[(x-n+1)..(x)]], filter (\x -> (not ((checkIfExists (Board map) x)))) (getEdgePosColumn (head (getFromListIndex [(inColumn n map color a y) | a<-[(x-n+1)..(x)]])) (last (getFromListIndex [(inColumn n map color a y) | a<-[(x-n+1)..(x)]])))]

getEdgePosColumn (Pos(fx,fy)) (Pos(lx,ly))= [Pos(fx-1,fy), Pos(lx+1,ly)]
   
checkRowInArea n emptys map color (Pos(x,y)) = getFromListIndex [(inRow n map color x b) | b<-[(y-n+1)..(y)]] 
checkRightDiagonalInArea n emptys map color (Pos(x,y))= getFromListIndex [(inRightDiagonal n map color (x-k) (y-k)) | k<-[0..(n-1)]]
checkLeftDiagonalInArea n emptys map color (Pos(x,y))= getFromListIndex [(inLeftDiagonal n map color (x+k) (y-k)) | k<-[0..(n-1)]]

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



getFromListIndex list 
    | getIndexOfPositions list == Nothing = []
    | getIndexOfPositions list /= Nothing = list !! getIndexFromJust (getIndexOfPositions list)

getIndexFromJust (Just index) = index
getIndexOfPositions list = findIndex (\x-> length x > 0) list
checkIfColor map pos color = (Map.lookup pos map == Just color)
checkIfNotEmpty list
    | list == [] = False






--TESTS--
testMap1 = Map.fromList ([(Pos(x,y), W)| x <-[8..13], y<-[4..7]]++[(Pos(12,8), W),(Pos(12,3), W)]++[(Pos(7,4), B)])

testMap2 = Map.fromList ([(Pos(5+k,6+k), B)| k <-[0..3]]++[(Pos(5+k,4+k), B)| k <-[0..3]]++[(Pos(5+k,5+k), W)| k <-[0..4]])



--END--












--quick check




 
--testTree :: Tree Integer
--testTree = Node 1 [ Node 2 [ Node 4 [ Node 6 [], Node 8 [] ],
 --                            Node 5 [ Node 7 [], Node 9 [] ] ],
 --                   Node 3 [ Node 10 [], 
 --                            Node 11 [] ] 
 --                 ]



--padding :: (Num a) => a -> String
--padding 0 = ""
--padding n = " " ++ padding(n-1)

--showTree :: (Show a, Num b) => Tree a -> b -> String
--showTree Leaf n = (padding n) ++ "."
--showTree (Node a l r ) n = let showl = showTree l (n+4) in
--                           let showr = showTree r (n+4) in
--                           let showc = (padding n) ++ (show a) in
--                           showc ++ "\n" ++ showl ++ "\n" ++ showr

--instance (Show a) => Show (Tree a) where
--    show a = showTree a 0



--data Cell = C Color Position deriving Show

--gameMap :: Integer -> Map Integer [Integer]
--gameMap n = Map.fromList (map makePair [1..n])
-- where makePair x = (x, [1..n])

--(!) :: Position -> Color

