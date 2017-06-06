--- cabal: parsec3, parsec-numbers
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


parsePosI :: Parser Int
parsePosI = do
            x <- int
            if (x<1 || x>19) then
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
    Right pos -> (hPutStrLn stderr $ "ruch = " ++ (show pos))
    Left x -> fail $ show x


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

-- main = putStrLn $ show $ length $ levels $ gameTree $ initGame 3
main = doPlay
