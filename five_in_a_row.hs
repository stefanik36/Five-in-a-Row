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

tBoard = Board bMap

tmpMap = Map.fromList [(Pos(x,y), W)| x <-[8..12], y<-[4..7]]

instance Show (Board) where
    show x = showBoard x

showBoard :: (Board) -> [Char]
showBoard (Board m) = "\n"++intercalate "" [((showCell m (Pos(x, y)))++nextRow y)| x <-cords, y<-cords]



showCell m pos
    | Map.lookup pos m == Nothing = " - "
    | Map.lookup pos m == Just B = " "++ (show B) ++" "
    | Map.lookup pos m == Just W = " "++ (show W) ++" "


nextRow y 
    | y /= mapRows = ""
    | y == mapRows = "\n"

getMap (Board(m)) = m

--insertCellToMap pos c bMap = Map.insert pos c bMap

insertCell pos c (Board map) = Board (Map.insert pos c map)


-- #lab4

data Game = Game Board Color deriving Show--Int


tGame = Game tBoard B
--Game -> [Game]
--f: Board->C -> [Board]

checkIfExists (Board map) pos = Map.lookup pos map == Nothing

getOppositeColor c 
    | c == B = W
    | c == W = B


posibleMoves (Game board color) =  [(Game (insertCell (Pos(x,y)) color board) (getOppositeColor color) )| x<-cords, y<-cords, checkIfExists board (Pos(x,y))]


--createTree (Game (Board m) color)
--    | m == Map.fromList [] = []


createTree (Game board color) = Tree.Node (Game board color) (map createTree (posibleMoves (Game board color)))


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

