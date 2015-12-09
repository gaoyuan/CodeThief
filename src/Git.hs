module Git where

import System.Process

gitAddAll :: String
gitAddAll = "git add ."

gitCommit :: String -> String
gitCommit msg = "git commit -m '" ++ msg ++ "'"

gitPush :: String
gitPush = "git push origin HEAD"

execAndWait :: String -> IO ()
execAndWait cmd = do
  (_, _, _, handle) <- createProcess $ shell cmd
  waitForProcess handle
  return ()

addCommitPush :: String -> IO ()
addCommitPush msg = do
  execAndWait gitAddAll
  execAndWait $ gitCommit msg
  execAndWait gitPush
