{-# LANGUAGE FlexibleContexts #-}
module Player where

import Control.Concurrent (threadDelay)
import Control.Monad.Random (MonadRandom(getRandomR))
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.State (MonadIO(liftIO), evalStateT, get, put)
import Data.List (find)
import System.IO (hFlush, stdout)

import Board
import Record

type Player m = Piece -> Board -> m Position

human :: MonadIO m => Player m
human piece board = liftIO $ do
  putStrLn $ "Your turn: " ++ show piece
  putStr "Input hand: "
  hFlush stdout  -- force `putStr` before `readLn`
  readLn

randomAI :: (MonadIO m, MonadRandom m) => Player m
randomAI _piece _board = do
  liftIO $ threadDelay 1000000
  getRandomR (1,9)

serialAI :: Monad m => Player m
serialAI _piece board = evalStateT serialAI' (cycle [1..9])
  where
    serialAI' = do
      i:is <- get
      put is
      if canPlace i board
        then return i
        else serialAI'

-- たぶん一番古い同一局面の手を出す
recordAI :: (MonadIO m, MonadRandom m, MonadReader [Record] m) => Player m
recordAI currentPiece currentBoard = do
  records <- ask

  case find isSameBoard records of
    Nothing -> randomAI currentPiece currentBoard
    Just (_, pos, _, _) -> return pos

  where
    isSameBoard :: Record -> Bool
    isSameBoard (piece, _, previousBoard, _) =
      piece == currentPiece && previousBoard == currentBoard
