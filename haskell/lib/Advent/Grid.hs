module Advent.Grid (Point2, Direction(..), Turn(..), parseDirection, move, turn, manhattan) where

import Advent.CyclicEnum (CyclicEnum(..))
import Advent.Parsing (Parser)
import Control.Applicative (asum)
import Text.Megaparsec (single)

type Point2 = (Int, Int)

data Direction = U | R | D | L deriving (Eq, Enum, Bounded)
instance CyclicEnum Direction

data Turn = TRight | TRound | TLeft | TNone

parseDirection :: Parser Direction
parseDirection = asum [U <$ single 'U', R <$ single 'R', D <$ single 'D', L <$ single 'L']

turn :: Turn -> Direction -> Direction
turn TRight = csucc
turn TRound = csucc . csucc
turn TLeft = cpred
turn TNone = id

move :: Direction -> Point2 -> Point2
move d (y, x) = case d of
    U -> (y - 1, x)
    R -> (y, x + 1)
    D -> (y + 1, x)
    L -> (y, x - 1)

manhattan :: Point2 -> Int
manhattan (x, y) = abs x + abs y
