module Player where

import Control.Monad.Random
import Control.Monad.State
import System.IO (hFlush, stdout)

import Board

type Player m = Board -> m Position
-- type Player m = Piece -> Board -> m Position  -- know which turn

human :: MonadIO m => Player m
human board = liftIO $ do
  putStr $ showBoard board
  putStr "Input hand: "
  hFlush stdout  -- force `putStr` before `readLn`
  readLn

randomAI :: MonadRandom m => Player m
randomAI _board = getRandomR (1,9)

serialAI :: Monad m => Player m
serialAI board = evalStateT serialAI' (cycle [1..9])
  where
    serialAI' = do
      i:is <- get
      put is
      if canPlace i board
        then return i
        else serialAI'
