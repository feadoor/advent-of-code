module Advent (solve, rawInput, inputLines, PlainString(..)) where

import Advent.Parsing (parseInt, parse, Parser)
import System.Environment (getProgName)
import Text.Megaparsec (single)
import Text.Printf (printf)

parseYearDay :: Parser (Int, Int)
parseYearDay = (,) <$> parseInt <* single '_' <*> parseInt

yearDay :: IO (Int, Int)
yearDay = parse parseYearDay <$> getProgName

rawInput :: IO String
rawInput = do
    (year, day) <- yearDay
    readFile $ printf "../inputs/%d/%02d.txt" year day

inputLines :: IO [String]
inputLines = lines <$> rawInput

newtype PlainString = PlainString String
instance Show PlainString where show (PlainString s) = s

solve :: (Show s, Show t) => IO a -> (a -> s) -> (a -> t) -> IO ()
solve readi p1 p2 = do
    input <- readi
    print $ p1 input
    print $ p2 input
