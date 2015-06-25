{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Main where

import Control.Monad.Random
import Control.Monad.State

import Board
import Player

main :: IO ()
main = do
  (board, result) <- evalStateT (game human ai) initialGameState
  let printResult message = putStr (showBoard board) >> putStrLn message
  printResult $ maybe "Draw." (\piece -> show piece ++ " won!") result
  where
    -- ai = serialAI
    ai = randomAI
    -- ai = human


data GameState = GameState { board :: Board, active :: Piece }

initialGameState :: GameState
initialGameState = GameState emptyBoard O

-- game :: (MonadState GameState m, MonadWriter [Board] m) => Player m -> Player m -> m ()
game :: (MonadIO m, MonadState GameState m) => Player m -> Player m -> m (Board, Maybe Piece)
game p1 p2 = do
  board <- gets board
  liftIO $ putStr $ showBoard board
  piece <- gets active
  pos <- p1 piece board
  if not $ canPlace pos board
    then game p1 p2
    else do
      let board' = updateBoard pos piece board
      modify $ \s -> GameState { board = board', active = change piece }
      if
        | isWon board'  -> return (board', Just piece)
        | isFull board' -> return (board', Nothing)
        | otherwise     -> game p2 p1