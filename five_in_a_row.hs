--import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

data Color = B | W deriving Eq
newtype Position = Pos (Int, Int) deriving (Show, Eq)
newtype Board a = CBoard a
--newtype Null = Null deriving Show 

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

mapRows = 3
cords = [1..mapRows]


bMap = Map.fromList [(Pos(x,y), B)| x <-cords, y<-cords]

numberOfCells = (Map.size bMap)

tPos = Pos(1,2)

instance (Show b) => Show (Board b) where
    show x = showBoard x



showBoard (CBoard m) = intercalate "" [(showCell m x y)| x <-cords, y<-cords]

tmp = CBoard bMap

foo m x y
    | Map.lookup (Pos(x,y)) m == Nothing = " _ "
    | Map.lookup (Pos(x,y)) m == Just B = " "++ (show B) ++" "
    | Map.lookup (Pos(x,y)) m == Just W = " "++ (show W) ++" "

--foo2 m x y = let showN = Map.lookup (Pos(x,y)) m in
--    (Maybe e)

showCell m x y
    | False == True = " _ "++checkWidth y++checkWidth x
    | True == True = " c "++checkWidth y





checkWidth y 
    | y /= mapRows = ""
    | y == mapRows = "\n"






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

