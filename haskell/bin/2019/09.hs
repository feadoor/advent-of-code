import Advent (rawInput, solve)
import Advent.Intcode (Memory, parseMemory, run, vm)
import Advent.Parsing (parse)

readInput :: IO Memory
readInput = parse parseMemory <$> rawInput

part1 :: Memory -> Int
part1 = head . (`run` [1]) . vm

part2 :: Memory -> Int
part2 = head . (`run` [2]) . vm

main :: IO ()
main = solve readInput part1 part2
