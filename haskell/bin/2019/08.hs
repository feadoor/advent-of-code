import Advent (rawInput, solve, PlainString(..))
import Advent.Parsing (Parser, parse)
import Data.Char (digitToInt)
import Data.List (transpose, minimumBy)
import Data.Ord (comparing)
import Text.Megaparsec (many)
import Text.Megaparsec.Char (digitChar)

rows :: Int
rows = 25

cols :: Int
cols = 6

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

layers :: [a] -> [[a]]
layers = chunks $ rows * cols

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

squash :: [[Int]] -> [Int]
squash = map (head . filter (/= 2)) . transpose

arrange :: [a] -> [[a]]
arrange = chunks rows

parsePixels :: Parser [Int]
parsePixels = map digitToInt <$> many digitChar

readInput :: IO [[Int]]
readInput = layers . parse parsePixels <$> rawInput

part1 :: [[Int]] -> Int
part1 = ((*) <$> count 1 <*> count 2) . minimumBy (comparing $ count 0)

part2 :: [[Int]] -> String
part2 = unlines . arrange . map charFor . squash
    where charFor 0 = ' '
          charFor _ = '#'

main :: IO ()
main = solve readInput part1 (PlainString . part2)
