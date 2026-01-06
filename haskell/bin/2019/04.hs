import Advent.Parsing (Parser, parse, parseInt)
import Data.List (group)
import Text.Megaparsec (single)
import Advent (rawInput, solve)

parseRange :: Parser (Int, Int)
parseRange = (,) <$> parseInt <* single '-' <*> parseInt

pairs :: [a] -> [(a, a)]
pairs = zip <$> id <*> tail

increasing :: Ord a => [a] -> Bool
increasing = all ((<=) <$> fst <*> snd) . pairs

hasDuplicate :: Eq a => [a] -> Bool
hasDuplicate = any ((>= 2) . length) . group

hasExactDuplicate :: Eq a => [a] -> Bool
hasExactDuplicate = any ((== 2) . length) . group

readInput :: IO (Int, Int)
readInput = parse parseRange <$> rawInput

part1 :: (Int, Int) -> Int
part1 (x, y) = length . filter ((&&) <$> increasing <*> hasDuplicate) $ map show [x..y]

part2 :: (Int, Int) -> Int
part2 (x, y) = length . filter ((&&) <$> increasing <*> hasExactDuplicate) $ map show [x..y]

main :: IO ()
main = solve readInput part1 part2
