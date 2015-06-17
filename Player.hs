module Player where

import Control.Monad.Random
import Control.Monad.State
import System.IO (hFlush, stdout)

import Board

type Player m = Piece -> Board -> m Position

human :: MonadIO m => Player m
human piece board = liftIO $ do
  putStr $ showBoard board
  putStrLn $ "Your turn: " ++ show piece
  putStr "Input hand: "
  hFlush stdout  -- force `putStr` before `readLn`
  readLn

randomAI :: MonadRandom m => Player m
randomAI _piece _board = getRandomR (1,9)

serialAI :: Monad m => Player m
serialAI _piece board = evalStateT serialAI' (cycle [1..9])
  where
    serialAI' = do
      i:is <- get
      put is
      if canPlace i board
        then return i
        else serialAI'
