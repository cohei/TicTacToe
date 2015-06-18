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
main = evalStateT (game human ai) initialGameState
  where
    -- ai = serialAI
    ai = randomAI
    -- ai = human


data GameState = GameState { board :: Board, active :: Piece }

initialGameState :: GameState
initialGameState = GameState emptyBoard O

-- type Winner = Piece
-- game :: (MonadState GameState m) => Player m -> Player m -> m Winner
-- game :: (MonadState GameState m, MonadWriter [Board] m) => Player m -> Player m -> m Winner
game :: (MonadIO m, MonadState GameState m) => Player m -> Player m -> m ()
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
      let printResult message = liftIO $ putStr (showBoard board') >> putStrLn message
      if
        | isWon board'  -> printResult $ show piece ++ " won!"
        | isFull board' -> printResult "Draw."
        | otherwise     -> game p2 p1
