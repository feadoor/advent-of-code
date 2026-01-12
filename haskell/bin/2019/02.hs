import Advent (solve, rawInput)
import Advent.Intcode (Memory, finalState, vm, parseMemory)
import Advent.Parsing (parse)
import qualified Data.Sequence as Seq

setup :: Int -> Int -> Memory -> Memory
setup a b = Seq.update 1 a . Seq.update 2 b

result :: Int -> Int -> Memory -> Int
result a b = (`Seq.index` 0) . (`finalState` []) . vm . setup a b

find :: Int -> Memory -> (Int, Int)
find x m = let result' (a, b) = result a b m
           in head . filter ((== x) . result') $ liftA2 (,) [0..99] [0..99]

readInput :: IO Memory
readInput = parse parseMemory <$> rawInput

part1 :: Memory -> Int
part1 = result 12 2

part2 :: Memory -> Int
part2 = (\(a, b) -> 100 * a + b) . find 19690720

main :: IO ()
main = solve readInput part1 part2
