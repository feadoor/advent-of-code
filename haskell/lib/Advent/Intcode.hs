{-# Language RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Advent.Intcode (Memory, Effect(..), parseMemory, vm, effect, run, finalState) where
    
import Advent.Parsing (Parser, parseInt)
import Data.Sequence (Seq, (><), index)
import qualified Data.Sequence as Seq
import Text.Megaparsec (sepBy, single)

type Memory = Seq Int

(!>>) :: Memory -> Int -> Int
(!>>) m i = if length m <= i then 0 else m `index` i

(<<!) :: Memory -> Int -> Int -> Memory
(<<!) m i x = Seq.update i x newM
    where newM = if length m <= i then m >< Seq.replicate (i - length m + 1) 0 else m

data Param = Position Int | Immediate Int | Relative Int

data Vm = Vm { pc :: Int, mem :: Memory, base :: Int }

opcode :: Vm -> Int
opcode Vm { .. } = (mem !>> pc) `mod` 100

mode :: Int -> Vm -> Int
mode i Vm { .. } = (mem !>> pc) `div` (10 ^ (i + 1)) `mod` 10

param :: Int -> Vm -> Param
param i v@Vm { .. } = let arg = mem !>> (pc + i) in case mode i v of
    0 -> Position arg
    1 -> Immediate arg
    2 -> Relative arg
    x -> error $ "Invalid parameter mode " ++ show x ++ " at " ++ show pc

get :: Param -> Vm -> Int
get p Vm { .. } = case p of
    Position arg -> mem !>> arg
    Immediate arg -> arg
    Relative arg -> mem !>> (base + arg)

set :: Param -> Int -> Vm -> Vm
set p x v@Vm { .. } = v { mem = (mem <<! i) x }
    where i = case p of
            Position arg -> arg
            Relative arg -> base + arg
            Immediate _ -> error $ "Attempt to write to immediate parameter at " ++ show pc

tick :: Int -> Vm -> Vm
tick n v@Vm { .. } = v { pc = pc + n }

jump :: Int -> Vm -> Vm
jump n v = v { pc = n }

adjustBase :: Int -> Vm -> Vm
adjustBase n v@Vm { .. } = v { base = base + n }

op1 :: Vm -> Vm
op1 v = let [p1, p2, p3] = map (`param` v) [1..3]
        in tick 4 . set p3 (get p1 v + get p2 v) $ v

op2 :: Vm -> Vm
op2 v = let [p1, p2, p3] = map (`param` v) [1..3]
        in tick 4 . set p3 (get p1 v * get p2 v) $ v

op3 :: Vm -> Int -> Vm
op3 v input = let p1 = param 1 v 
              in tick 2 . set p1 input $ v

op4 :: Vm -> (Int, Vm)
op4 v = let p1 = param 1 v
        in (get p1 v, tick 2 v)

op5 :: Vm -> Vm
op5 v = let [p1, p2] = map (`param` v) [1..2]
            cond = get p1 v /= 0
            next = if cond then jump (get p2 v) else tick 3
        in next v

op6 :: Vm -> Vm
op6 v = let [p1, p2] = map (`param` v) [1..2]
            cond = get p1 v == 0
            next = if cond then jump (get p2 v) else tick 3
        in next v

op7 :: Vm -> Vm
op7 v = let [p1, p2, p3] = map (`param` v) [1..3]
            n = if get p1 v < get p2 v then 1 else 0
        in tick 4 . set p3 n $ v

op8 :: Vm -> Vm
op8 v = let [p1, p2, p3] = map (`param` v) [1..3]
            n = if get p1 v == get p2 v then 1 else 0
        in tick 4 . set p3 n $ v

op9 :: Vm -> Vm
op9 v = let p1 = param 1 v
        in tick 2 . adjustBase (get p1 v) $ v

data Step = Step Vm | StepIn (Int -> Vm) | StepOut (Int, Vm) | StepHalt Memory

step :: Vm -> Step
step v = case opcode v of
    1  -> Step     $ op1 v
    2  -> Step     $ op2 v
    3  -> StepIn   $ op3 v
    4  -> StepOut  $ op4 v
    5  -> Step     $ op5 v
    6  -> Step     $ op6 v
    7  -> Step     $ op7 v
    8  -> Step     $ op8 v
    9  -> Step     $ op9 v
    99 -> StepHalt $ mem v
    x  -> error $ "Unknown opcode " ++ show x ++ " at " ++ show (pc v)

data Effect = Input (Int -> Effect) | Output (Int, Effect) | Halt Memory

parseMemory :: Parser Memory
parseMemory = Seq.fromList <$> parseInt `sepBy` single ','

vm :: Memory -> Vm
vm m = Vm { pc = 0, mem = m, base = 0 }

effect :: Vm -> Effect
effect v = case step v of
    Step v'           -> effect v'
    StepIn f          -> Input (effect . f)
    StepOut (out, v') -> Output (out, effect v')
    StepHalt m        -> Halt m

run' :: Effect -> [Int] -> [Int]
run' ef inputs = case ef of
    Input f           -> run' (f $ head inputs) $ tail inputs
    Output (out, ef') -> out : run' ef' inputs
    Halt _            -> []

run :: Vm -> [Int] -> [Int]
run = run' . effect

finalState' :: Effect -> [Int] -> Memory
finalState' ef inputs = case ef of
    Input f         -> finalState' (f $ head inputs) $ tail inputs
    Output (_, ef') -> finalState' ef' inputs
    Halt m          -> m

finalState :: Vm -> [Int] -> Memory
finalState = finalState' . effect
