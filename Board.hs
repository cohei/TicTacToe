module Board
  ( Board
  , Piece(..)
  , Position
  , isWon
  , emptyBoard
  , showBoard
  , updateBoard
  , canPlace
  , change
  , isFull
  ) where

import Data.Maybe (isJust)
import Data.List (intercalate, intersperse, transpose)
import Data.List.Split (chunksOf)

data Piece = X | O deriving (Eq, Show, Ord)

change :: Piece -> Piece
change X = O
change O = X

{-
 1 | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9
-}
type Position = Int
type Board = [Maybe Piece]

showBoard :: Board -> String
showBoard =
  unlines .
  intersperse "---+---+---" .
  map (intercalate "|") .
  chunksOf 3 .
  map (\p -> " " ++ maybe " " show p ++ " ")

emptyBoard :: Board
emptyBoard = replicate 9 Nothing

sampleBoard :: Board
sampleBoard = take 9 $ cycle [Nothing, Just X, Just O, Nothing]

isFull :: Board -> Bool
isFull = all isJust

isWon :: Board -> Bool
isWon b = any full $ rows ++ cols ++ diagonals
  where
    full ps@[p1,p2,p3] = all isJust ps && p1 == p2 && p2 == p3
    rows               = chunksOf 3 b
    cols               = transpose rows
    diagonals          = map (map (b !!)) [[0,4,8], [2,4,6]]

isFinished :: Board -> Bool
isFinished board = isFull board || isWon board

updateBoard :: Position -> Piece -> Board -> Board
updateBoard position piece board = take (position - 1) board ++ [Just piece] ++ drop position board

canPlace :: Position -> Board -> Bool
canPlace position board = not $ isJust $ board !! (position - 1)
