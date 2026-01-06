import Advent (inputLines, solve)
import Advent.Parsing (Parser, parseMany)
import Data.Functor ((<&>))
import Data.Map (Map, (!))
import Data.Monoid (Sum(Sum, getSum))
import Data.Set (difference, union)
import Data.Tuple (swap)
import Text.Megaparsec (many, single)
import Text.Megaparsec.Char (alphaNumChar)
import qualified Data.Map as Map
import qualified Data.Set as Set

parseName :: Parser String
parseName = many alphaNumChar

parseOrbit :: Parser (String, String)
parseOrbit = (,) <$> parseName <* single ')' <*> parseName

createTree :: [(String, String)] -> Map String String
createTree = Map.fromList . map swap

accumulation :: (Ord a, Monoid m) => (a -> m) -> Map a a -> Map a m
accumulation f t = t <&> \p -> case Map.lookup p (accumulation f t) of
    Nothing  -> f p
    Just acc -> f p <> acc

readInput :: IO (Map String String)
readInput = createTree . parseMany parseOrbit <$> inputLines

part1 :: Map String String -> Int
part1 = getSum . sum . accumulation (const $ Sum 1)

part2 :: Map String String -> Int
part2 t = let ancestors = accumulation Set.singleton t
              youAncestors = ancestors ! "YOU"
              sanAncestors = ancestors ! "SAN"
              uniqueAncestors = (youAncestors `difference` sanAncestors) `union` (sanAncestors `difference` youAncestors)
          in length uniqueAncestors

main :: IO ()
main = solve readInput part1 part2
