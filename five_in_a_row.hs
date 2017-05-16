--import Data.Map (Map)
import qualified Data.Map as Map

data Color = B | W 
newtype Position = Pos (Int, Int) deriving (Show, Eq)

instance Ord Position where
compare (Pos(l1,r1)) (Pos(l2,r2)) 
| l1 == l2 && r1==r2 = EQ
| l1 == l2 && r1 | l1 == l2 && r1>r2 = GT
| l1 < l2 = LT
| l1 > l2 = GT

--data Cell = C Color Position deriving Show

--gameMap :: Integer -> Map Integer [Integer]
--gameMap n = Map.fromList (map makePair [1..n])
-- where makePair x = (x, [1..n])

--(!) :: Position -> Color

instance Show Color where
show B = "x"
show W = "o"
--TODO board show
tmpMap2 = Map.fromList [(Pos(x,y), B)| x <-[1..3], y<-[1..3]]
