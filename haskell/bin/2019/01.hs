import Advent (inputLines, solve)
import Advent.Parsing (parseMany, parseInt)

fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)

iteratedFuel :: Int -> Int
iteratedFuel = sum . drop 1 . takeWhile (>0) . iterate fuel

readInput :: IO [Int]
readInput = parseMany parseInt <$> inputLines

part1 :: [Int] -> Int
part1 = sum . map fuel

part2 :: [Int] -> Int
part2 = sum . map iteratedFuel

main :: IO ()
main = solve readInput part1 part2
