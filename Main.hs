{-# LANGUAGE FlexibleContexts #-}
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
initialGameState = GameState emptyBoard X

-- type Winner = Piece
-- game :: (MonadState GameState m) => Player m -> Player m -> m Winner
-- game :: (MonadState GameState m, MonadWriter [Board] m) => Player m -> Player m -> m Winner
game :: (MonadState GameState m) => Player m -> Player m -> m ()
game p1 p2 = do
  board <- gets board
  pos <- p1 board
  if not $ canPlace pos board
    then game p1 p2
    else do
      piece <- gets active
      let board' = updateBoard pos piece board
      modify $ \s -> GameState { board = board', active = change piece }
      if isFull board' || isWon board'
        then return ()
        else game p2 p1
