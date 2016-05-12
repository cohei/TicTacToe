{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Main where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (MonadState, evalStateT, gets, modify)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Monad.Writer (MonadWriter(tell), runWriterT)

import Board
import Player
import Record

main :: IO ()
main = do
  records <- readRecord
  ((board, result), records) <- flip runReaderT records $ runWriterT $ evalStateT (game human ai) initialGameState
  writeRecord records
  printResult board result
  where
    -- ai = serialAI
    -- ai = randomAI
    ai = recordAI
    -- ai = human

printResult :: Board -> Maybe Piece -> IO ()
printResult board result = do
  putStr $ showBoard board
  putStrLn $ maybe "Draw." ((++ " won!") . show) result

data GameState = GameState { board :: Board, active :: Piece }

initialGameState :: GameState
initialGameState = GameState emptyBoard O

game ::
  (MonadIO m, MonadState GameState m, MonadWriter [Record] m) =>
  Player m ->
  Player m ->
  m (Board, Maybe Piece)
game p1 p2 = do
  board <- gets board
  liftIO $ putStr $ showBoard board
  piece <- gets active
  pos <- p1 piece board
  if not $ canPlace pos board
    then game p1 p2
    else do
      let board' = updateBoard pos piece board
      tell $ pure (piece, pos, board, board')
      modify $ const GameState { board = board', active = flipPiece piece }
      if
        | isWon board'  -> return (board', Just piece)
        | isFull board' -> return (board', Nothing)
        | otherwise     -> game p2 p1
