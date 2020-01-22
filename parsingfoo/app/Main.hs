module Main where

import Data.Char (intToDigit)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Attoparsec.Text (many', manyTill, eitherP, endOfLine, anyChar, notChar, digit, char, parseOnly)
import Data.Attoparsec.Types (Parser)
import Data.Text (pack)

type Index = (Int, Int)
data Sudoku = Sudoku (Map Index Int)

instance Show Sudoku where
  show (Sudoku fields) = "/---v---v---\\\n"
    ++ (format '|' (1,1) (1,2) (1,3) '|' (1,4) (1,5) (1,6) '|' (1,7) (1,8) (1,9) '|')
    ++ (format '|' (2,1) (2,2) (2,3) '|' (2,4) (2,5) (2,6) '|' (2,7) (2,8) (2,9) '|')
    ++ (format '|' (3,1) (3,2) (3,3) '|' (3,4) (3,5) (3,6) '|' (3,7) (3,8) (3,9) '|')
    ++ (">---+---+---<\n")
    ++ (format '|' (4,1) (4,2) (4,3) '|' (4,4) (4,5) (4,6) '|' (4,7) (4,8) (4,9) '|')
    ++ (format '|' (5,1) (5,2) (5,3) '|' (5,4) (5,5) (5,6) '|' (5,7) (5,8) (5,9) '|')
    ++ (format '|' (6,1) (6,2) (6,3) '|' (6,4) (6,5) (6,6) '|' (6,7) (6,8) (6,9) '|')
    ++ (">---+---+---<\n")
    ++ (format '|' (7,1) (7,2) (7,3) '|' (7,4) (7,5) (7,6) '|' (7,7) (7,8) (7,9) '|')
    ++ (format '|' (8,1) (8,2) (8,3) '|' (8,4) (8,5) (8,6) '|' (8,7) (8,8) (8,9) '|')
    ++ (format '|' (9,1) (9,2) (9,3) '|' (9,4) (9,5) (9,6) '|' (9,7) (9,8) (9,9) '|')
    ++ ("\\---^---^---/\n")
    where
      format ::
        Char -> Index -> Index -> Index ->
        Char -> Index -> Index -> Index ->
        Char -> Index -> Index -> Index -> Char -> String
      format s1 a b c s2 d e f s3 g h i s4 =
        s1:(char $ Map.lookup a fields):(char $ Map.lookup b fields):(char $ Map.lookup c fields):
        s2:(char $ Map.lookup d fields):(char $ Map.lookup e fields):(char $ Map.lookup f fields):
        s3:(char $ Map.lookup g fields):(char $ Map.lookup h fields):(char $ Map.lookup i fields):
        s4:"\n"
        where
          char :: Maybe Int -> Char
          char Nothing = ' '
          char (Just number) = intToDigit number

type Item = Either Char Char

data Line = Line Item Item Item Item Item Item Item Item Item
  deriving Show

lineParser = do
  many' $ char '|'
  i1 <- eitherP digit (char ' ')
  i2 <- eitherP digit (char ' ')
  i3 <- eitherP digit (char ' ')
  many' $ char '|'
  i4 <- eitherP digit (char ' ')
  i5 <- eitherP digit (char ' ')
  i6 <- eitherP digit (char ' ')
  many' $ char '|'
  i7 <- eitherP digit (char ' ')
  i8 <- eitherP digit (char ' ')
  i9 <- eitherP digit (char ' ')
  many' $ char '|'
  return $ Line i1 i2 i3 i4 i5 i6 i7 i8 i9

sudokuParser = do
  manyTill anyChar (char '|')
  l1 <- lineParser
  many' endOfLine
  l2 <- lineParser
  many' endOfLine
  l3 <- lineParser
  manyTill anyChar (char '|')
  l4 <- lineParser
  many' endOfLine
  l5 <- lineParser
  many' endOfLine
  l6 <- lineParser
  manyTill anyChar (char '|')
  l7 <- lineParser
  many' endOfLine
  l8 <- lineParser
  many' endOfLine
  l9 <- lineParser
  manyTill anyChar (char '\n')
  return $ (l1, l2, l3, l4, l5, l6, l7, l8, l9)

main :: IO ()
main = do
  putStrLn $ show $ Sudoku $ Map.fromList
    [ ((1, 2), 3), ((1, 5), 7), ((2, 1), 6), ((2, 4), 1), ((2, 5), 9)
    , ((2, 6), 5), ((3, 2), 9), ((3, 3), 8), ((3, 8), 6), ((4, 1), 8)
    , ((4, 5), 6), ((4, 9), 3), ((5, 1), 4), ((5, 4), 8), ((5, 6), 3)
    , ((5, 9), 1), ((6, 1), 7), ((6, 5), 2), ((6, 9), 6), ((7, 2), 6)
    , ((7, 7), 2), ((7, 8), 8), ((8, 4), 4), ((8, 5), 1), ((8, 6), 9)
    , ((8, 9), 5), ((9, 5), 8), ((9, 8), 7)]
  putStrLn $ show $ parseOnly sudokuParser (pack "/---v---v---\\\n| 3 | 7 |   |\n|6  |195|   |\n| 98|   | 6 |\n>---+---+---<\n|8  | 6 |  3|\n|4  |8 3|  1|\n|7  | 2 |  6|\n>---+---+---<\n| 6 |   |28 |\n|   |419|  5|\n|   | 8 | 7 |\n\\---^---^---/\n")
  putStrLn $ show $ parseOnly sudokuParser (pack "| 3 | 7 |   |\n|6  |195|   |\n| 98|   | 6 |\n|8  | 6 |  3|\n|4  |8 3|  1|\n|7  | 2 |  6|\n| 6 |   |28 |\n|   |419|  5|\n|   | 8 | 7 |\n")
