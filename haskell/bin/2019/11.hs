{-# Language RecordWildCards #-}

import Advent (rawInput, solve, PlainString (PlainString))
import Advent.Grid (Turn(..), Point2, Direction(..), move, turn)
import Advent.Intcode (Memory, run, parseMemory, vm)
import Advent.Parsing (parse)
import Data.Set (Set)
import qualified Data.Set as Set

type Field = Set Point2

data RobotState = RobotState { pos :: Point2, dir :: Direction, field :: Field }

data Colour = B | W deriving Enum

colourCode :: Colour -> Int
colourCode = fromEnum

colour :: Int -> Colour
colour = toEnum

rotation :: Int -> Turn
rotation 0 = TLeft
rotation 1 = TRight
rotation x = error $ "Invalid rotation " ++ show x

pairs :: [a] -> [(a, a)]
pairs (x:y:xs) = (x, y) : pairs xs
pairs _        = []

paint :: Colour -> Point2 -> Field -> Field
paint B = Set.delete
paint W = Set.insert

inputColour :: Point2 -> Field -> Colour
inputColour pos field = if Set.member pos field then W else B

tick :: (Colour, Turn) -> RobotState -> RobotState
tick (c, r) RobotState { .. } = RobotState { pos = newPos, dir = newDir, field = newField }
    where newPos   = move newDir pos
          newDir   = turn r dir
          newField = paint c pos field

steps :: [(Colour, Turn)] -> RobotState -> [RobotState]
steps instrs state = scanl (flip tick) state instrs

instructions :: ([Int] -> [Int]) -> [Colour] -> [(Colour, Turn)]
instructions runner inputs = [(colour x, rotation y) | (x, y) <- pairs outputs]
    where outputs = runner (map colourCode inputs)

allStates :: ([Int] -> [Int]) -> RobotState -> [RobotState]
allStates runner startState = states
    where states = steps instrs startState
          instrs = instructions runner inputs
          inputs = map (inputColour <$> pos <*> field) states

readInput :: IO Memory
readInput = parse parseMemory <$> rawInput

part1 :: Memory -> Int
part1 mem = Set.size positions
    where positions = Set.fromList $ map pos $ allStates (run $ vm mem) startState
          startState = RobotState { pos = (0, 0), dir = U, field = Set.empty }

part2 :: Memory -> String
part2 mem = unlines [[pixel y x | x <- [minx..maxx]] | y <- [miny..maxy]]
    where pixel y x = if Set.member (y, x) painted then 'X' else ' '
          miny       = minimum . Set.map fst $ painted
          maxy       = maximum . Set.map fst $ painted
          minx       = minimum . Set.map snd $ painted
          maxx       = maximum . Set.map snd $ painted
          painted    = field . last $ allStates (run $ vm mem) startState
          startState = RobotState { pos = (0, 0), dir = U, field = Set.singleton (0, 0) }

main :: IO ()
main = solve readInput part1 (PlainString . part2)
