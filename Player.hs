module Player where

import Control.Concurrent (threadDelay)
import Control.Monad.Random
import Control.Monad.State
import System.IO (hFlush, stdout)

import Board

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
