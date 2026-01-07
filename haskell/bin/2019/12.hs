import Advent.Parsing (Parser, parseInt, parseMany)
import Data.List (elemIndex, transpose)
import Text.Megaparsec (many, sepBy, single, (<|>))
import Text.Megaparsec.Char (alphaNumChar)
import Advent (inputLines, solve)
import Data.Maybe (fromJust)

data Moon = Moon { pos :: [Int], vel :: [Int] } deriving Eq

makeMoon :: [Int] -> Moon
makeMoon p = Moon { pos = p, vel = map (const 0) p }

step :: [Moon] -> [Moon]
step moons = map (move . accel moons) moons

move :: Moon -> Moon
move moon = moon { pos = zipWith (+) <$> pos <*> vel $ moon }

accel :: [Moon] -> Moon -> Moon
accel moons moon = moon { vel = zipWith (+) (vel moon) accels }
    where accels = map (sum . map signum) deltas
          deltas = zipWith (map . subtract) (pos moon) (transpose $ map pos moons)

energy :: Moon -> Int
energy = (*) <$> kinetic <*> potential
    where kinetic   = sum . map abs . vel
          potential = sum . map abs . pos

flattened :: Moon -> [Moon]
flattened = map (\(p, v) -> Moon { pos = [p], vel = [v] }) . (zip <$> pos <*> vel)

period' :: [Moon] -> Maybe Int
period' moons = fmap (1 +) $ elemIndex moons $ drop 1 $ iterate step moons

period :: [Moon] -> Maybe Int
period moons = foldr1 (liftA2 lcm) periods
    where periods = map period' . transpose . map flattened $ moons

parseCoordinate :: Parser Int
parseCoordinate = alphaNumChar <* single '=' *> parseInt

parsePosition :: Parser [Int]
parsePosition = parseCoordinate `sepBy` many (single ',' <|> single ' ')

parseMoon :: Parser Moon
parseMoon = makeMoon <$> (single '<' *> parsePosition <* single '>')

readInput :: IO [Moon]
readInput = parseMany parseMoon <$> inputLines

part1 :: [Moon] -> Int
part1 moons = sum . map energy $ iterate step moons !! 1000

part2 :: [Moon] -> Int
part2 = fromJust . period

main :: IO ()
main = solve readInput part1 part2
