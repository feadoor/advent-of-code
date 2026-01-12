import Advent (inputLines, solve)
import Advent.Grid (Direction, parseDirection, manhattan, Point2, move)
import Advent.Parsing (Parser, parseInt, parseMany)
import Data.Map (Map)
import Text.Megaparsec (single, sepBy)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Instruction = Instruction Direction Int

parseInstruction :: Parser Instruction
parseInstruction = liftA2 Instruction parseDirection parseInt

parseWire :: Parser [Instruction]
parseWire = parseInstruction `sepBy` single ','

steps :: Instruction -> [Direction]
steps (Instruction d n) = replicate n d

positions :: [Instruction] -> [Point2]
positions = drop 1 . scanl (flip move) (0, 0) . concatMap steps

indexedPositions :: [Instruction] -> Map Point2 Int
indexedPositions = Map.fromListWith min . (`zip` [1..]) . positions

readInput :: IO [[Instruction]]
readInput = parseMany parseWire <$> inputLines

part1 :: [[Instruction]] -> Int
part1 = minimum . Set.map manhattan . foldr1 Set.intersection . map (Map.keysSet . indexedPositions)

part2 :: [[Instruction]] -> Int
part2 = minimum . foldr1 (Map.intersectionWith (+)) . map indexedPositions

main :: IO ()
main = solve readInput part1 part2
