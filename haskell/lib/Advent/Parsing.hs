module Advent.Parsing (Parser, parseInt, parseLines, parse, parseMany) where

import Data.Void
import qualified Text.Megaparsec as MP (Parsec, parse, errorBundlePretty, sepBy)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Char (eol)

type Parser = MP.Parsec Void String

parseInt :: Integral a => Parser a
parseInt = signed (return ()) decimal

parseLines :: Parser a -> Parser [a]
parseLines = (`MP.sepBy` eol)

parse :: Parser a -> String ->  a
parse p = either (error . MP.errorBundlePretty) id . MP.parse p ""

parseMany :: Parser a -> [String] -> [a]
parseMany p = map (parse p)
