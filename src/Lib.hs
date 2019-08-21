{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( toCSV ) where

import Data.Csv hiding (Parser)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import Data.Aeson hiding ((<?>))
import Data.Aeson.Encode.Pretty

import Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Control.Monad.Except
import Data.Functor.Identity

vecToJSON :: V.Vector [String] -> String
vecToJSON v =
  BLU.toString $ encodePretty $ 
  (\row -> object (zipWith (\c val -> (T.pack $ "c"++ show c,val))
           (Prelude.take (Prelude.length row) [1..])
           row)) <$> (fmap.fmap) (String. T.pack ) (V.toList v)

toCSV' :: String-> Either String String
toCSV' csv = vecToJSON <$> ((Data.Csv.decode NoHeader) $ BLU.fromString csv)

---

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell '"' <|> quotedCell '\'' <|> many (noneOf ",\n\r")
quotedCell quote = 
    do char quote
       content <- many $ quotedChar quote
       char quote <?> "quote at end of cell"
       return content

quotedChar quote =
        noneOf [quote]
    <|> try (string [quote,quote] >> return quote)

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

lToJSON :: [[String]] -> String
lToJSON v =
  BLU.toString $ encodePretty $ 
  (\row -> object (zipWith (\c val -> (T.pack $ "c"++ show c,val))
           (Prelude.take (Prelude.length row) [1..])
           row)) <$> (fmap.fmap) (String. T.pack ) v

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

toCSV :: String-> Either String String
toCSV csv = lToJSON <$> either (throwError . show) return (parseCSV csvData)
  where csvData = if (last csv /= '\n') then csv++"\n" else csv
