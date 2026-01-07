import Advent (inputLines, solve)
import Advent.Parsing (Parser, parseInt, parseMany)
import Data.Graph (reverseTopSort, graphFromEdges)
import Data.Map (Map, (!))
import Text.Megaparsec (single, many, sepBy)
import Text.Megaparsec.Char (alphaNumChar, string)
import qualified Data.Map as Map

type Ingredient = (Int, String)

type Recipe = (Ingredient, [Ingredient])

type Inventory = Map String Int

first :: (a, b, c) -> a
first (a, _, _) = a

topologicalOrder :: [Recipe] -> [Recipe]
topologicalOrder rs = map (first . nlookup) orderedVertices
    where orderedVertices = reverseTopSort graph
          (graph, nlookup, _) = graphFromEdges edgeList
          edgeList = map (\r -> (r, snd $ fst r, map snd $ snd r)) rs

update :: Int -> Ingredient -> Inventory -> Inventory
update qty ing = Map.alter f $ snd ing
    where f Nothing  = Just $ qty * fst ing
          f (Just x) = Just $ qty * fst ing + x

transmute :: Recipe -> Inventory -> Inventory
transmute r m = foldr1 (.) updates m
    where updates = map (update qty) $ snd r
          qty = ((m ! name) + amt - 1) `div` amt
          (amt, name) = fst r

oreRequiredForFuel :: [Recipe] -> Int -> Int
oreRequiredForFuel rs fuel = end ! "ORE"
    where end = foldr1 (.) (map transmute recipes) start
          start = Map.insert "FUEL" fuel Map.empty
          recipes = topologicalOrder rs

maximumFuelFromOre :: [Recipe] -> Int -> Int
maximumFuelFromOre rs ore = foldr doStep 0 steps
    where steps = takeWhile ((<= ore) . oreRequiredForFuel rs) $ iterate (*2) 1
          doStep s v = if oreRequiredForFuel rs (v + s) <= ore then v + s else v

parseIngredient :: Parser Ingredient
parseIngredient = (,) <$> parseInt <* single ' ' <*> many alphaNumChar

parseIngredients :: Parser [Ingredient]
parseIngredients = parseIngredient `sepBy` string ", "

parseRecipe :: Parser Recipe
parseRecipe = flip (,) <$> parseIngredients <* string " => " <*> parseIngredient

readInput :: IO [Recipe]
readInput = parseMany parseRecipe <$> inputLines

part1 :: [Recipe] -> Int
part1 = flip oreRequiredForFuel 1

part2 :: [Recipe] -> Int
part2 = flip maximumFuelFromOre 1000000000000

main :: IO ()
main = solve readInput part1 part2
