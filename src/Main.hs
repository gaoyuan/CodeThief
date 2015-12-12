module Main where

import File
import Git

main :: IO ()
main = do
  pullFilesToDisk
  addCommitPush "Feeling hard working today!"
