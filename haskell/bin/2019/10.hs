{-# Language TupleSections #-}

import Advent (rawInput, solve)
import Advent.Parsing (Parser, parseLines, parse)
import Data.List (maximumBy, sortBy, transpose)
import Data.Ord (comparing)
import Text.Megaparsec (many, single, (<|>))
import qualified Data.Map as Map
import qualified Data.Set as Set

relativePosition :: (Int, Int) -> (Int, Int) -> (Int, Int)
relativePosition (y, x) = (,) <$> (subtract y . fst) <*> (subtract x . snd)

relativeTo :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
relativeTo base = filter (/= (0, 0)) . map (relativePosition base)

gradient :: (Int, Int) -> (Int, Int)
gradient (y, x) = let g = abs $ gcd y x in (y `div` g, x `div` g)

angle :: (Int, Int) -> Double
angle (y, x) = let rawAngle = atan2 (fromIntegral x) (fromIntegral y) in pi - rawAngle

countVisibleFrom :: (Int, Int) -> [(Int, Int)] -> Int
countVisibleFrom (y, x) = length . Set.fromList . map gradient . relativeTo (y, x)

bestStation :: [(Int, Int)] -> (Int, Int)
bestStation asteroids = maximumBy (comparing $ flip countVisibleFrom asteroids) asteroids

destructionOrder :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
destructionOrder (y, x) asteroids = map (relativePosition (-y, -x)) flattenedAsteroids
    where flattenedAsteroids = concat $ transpose asteroidsInGradientOrder
          asteroidsInGradientOrder = map (byGradient Map.!) sortedGradients
          sortedGradients = sortBy (comparing angle) $ Map.keys byGradient
          byGradient = Map.fromListWith (++) $ map ((,) <$> gradient <*> pure) relativeAsteroids
          relativeAsteroids = relativeTo (y, x) asteroids

positionsInRow :: String -> [Int]
positionsInRow = map fst . filter ((== '#') . snd) . zip [0..]

coordinatesInRow :: Int -> String -> [(Int, Int)]
coordinatesInRow idx = map (idx,) . positionsInRow

coordinates :: [String] -> [(Int, Int)]
coordinates  = concatMap (uncurry coordinatesInRow) . zip [0..]

parseField :: Parser [String]
parseField = parseLines $ many (single '.' <|> single '#')

parseAsteroids :: Parser [(Int, Int)]
parseAsteroids = coordinates <$> parseField

readInput :: IO [(Int, Int)]
readInput = parse parseAsteroids <$> rawInput

part1 :: [(Int, Int)] -> Int
part1 asteroids = maximum . map (`countVisibleFrom` asteroids) $ asteroids

part2 :: [(Int, Int)] -> Int
part2 asteroids = 100 * x + y
    where (y, x) = destroyed !! 199
          destroyed = destructionOrder base asteroids
          base = bestStation asteroids

main :: IO ()
main = solve readInput part1 part2
