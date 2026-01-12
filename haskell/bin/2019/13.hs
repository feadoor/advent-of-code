import Advent (rawInput, solve)
import Advent.Intcode (Effect(..), Memory, parseMemory, run, vm, effect)
import Advent.Parsing (parse)
import qualified Data.Sequence as Seq

data BotState = BotState { ball :: Maybe Int, paddle :: Maybe Int, score :: Int }

everyNth :: Int -> [Int] -> [Int]
everyNth n xs = case drop (n - 1) xs of
    (x:xs') -> x : everyNth n xs'
    []      -> []

paddleDirection :: BotState -> Int
paddleDirection BotState { ball = Just b, paddle = Just p } = signum (b - p)
paddleDirection _ = 0

bot :: BotState -> Effect -> Int
bot state ef = case ef of
    Halt _  -> score state
    Input f -> bot state $ f $ paddleDirection state
    
    Output (-1, Output (0, Output (score', ef'))) -> bot state {score = score' } ef'
    Output (paddle', Output(_, Output (3, ef')))  -> bot state {paddle = Just paddle' } ef'
    Output (ball', Output(_, Output(4, ef')))     -> bot state { ball = Just ball' } ef'

    Output (_, Output (_, Output(_, ef'))) -> bot state ef'
    _                                      -> error "Outputs should come in threes!"

readInput :: IO Memory
readInput = parse parseMemory <$> rawInput

part1 :: Memory -> Int
part1 = length . filter (== 2) . everyNth 3 . (`run` []) . vm

part2 :: Memory -> Int
part2 = bot startState . effect . vm . Seq.update 0 2
    where startState = BotState { ball = Nothing, paddle = Nothing, score = 0 }

main :: IO ()
main = solve readInput part1 part2
