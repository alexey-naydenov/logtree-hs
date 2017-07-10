module Main where

import qualified LogTree.Lib as LTL

import System.Directory (getHomeDirectory)

main :: IO ()
main = do
  home <- getHomeDirectory
  LTL.mainFunction $ home ++ "/tmp/work/test.log.full"
