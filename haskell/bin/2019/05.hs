import Advent (rawInput, solve)
import Advent.Intcode (Memory, parseMemory, run, vm)
import Advent.Parsing (parse)

readInput :: IO Memory
readInput = parse parseMemory <$> rawInput

part1 :: Memory -> Int
part1 = last . (`run` [1]) . vm

part2 :: Memory -> Int
part2 = last . (`run` [5]) . vm

main :: IO ()
main = solve readInput part1 part2
