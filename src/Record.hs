module Record where

import Data.List (isPrefixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (listDirectory)

import Board

type Record = (Piece, Int, Board, Board)

writeRecord :: [Record] -> IO ()
writeRecord records = do
  t <- getPOSIXTime
  writeFile ("record-" ++ show t ++ ".txt") $ unlines $ map show records

readRecord :: IO [Record]
readRecord = map read . lines . concat <$> (mapM readFile =<< recordFiles)

recordFiles :: IO [FilePath]
recordFiles = filter ("record-" `isPrefixOf`) <$> listDirectory "."
