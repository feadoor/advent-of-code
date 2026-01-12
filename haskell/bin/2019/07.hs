import Advent.Intcode (Memory, vm, parseMemory, run)
import Advent.Parsing (parse)
import Advent (rawInput, solve)
import Data.Function (fix)
import Data.List (permutations)

amp :: Memory -> Int -> ([Int] -> [Int])
amp mem phase inputs = run (vm mem) (phase:inputs)

sequential :: [[Int] -> [Int]] -> [Int] -> [Int]
sequential = foldr1 (.)

looped :: [[Int] -> [Int]] -> Int -> [Int]
looped fs initial = fix (sequential fs . (initial :))

readInput :: IO Memory
readInput = parse parseMemory <$> rawInput

part1 :: Memory -> Int
part1 m = maximum . map (head . ($ [0]) . sequential . amps) $ permutations [0..4]
    where amps = map (amp m)

part2 :: Memory -> Int
part2 m = maximum . map (last . ($ 0) . looped . amps) $ permutations [5..9]
    where amps = map (amp m)

main :: IO ()
main = solve readInput part1 part2
