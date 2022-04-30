module Parser (parseStem, parseDesign) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Char (isLower, isUpper)
import Types

parseSize :: Parser Size
parseSize =
  (char 'L' >> return L)
    <|> (char 'S' >> return S)
    <?> "size must be one of L|S"

parseFlower :: Parser Char
parseFlower =
  anyChar >>= \flower ->
    if isLower flower
      then return flower
      else fail "Flower must be lower case"

parseStem :: Parser Stem
parseStem = parseFlower >>= \flower -> parseSize >>= \size -> return (Stem flower size)

parseStemAmount :: Parser StemAmount
parseStemAmount = decimal >>= \amount -> anyChar >>= \s -> return (StemAmount amount s)

parseDesign :: Parser Design
parseDesign = do
  name <- anyChar >>= \n -> if isUpper n then return n else fail "Design name must be upper case"
  size <- parseSize
  stems <- many1 parseStemAmount
  Design name size stems <$> decimal
