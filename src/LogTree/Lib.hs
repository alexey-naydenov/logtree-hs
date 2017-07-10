module LogTree.Lib
    ( mainFunction
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified LogTree.Utils as U

mainFunction :: String -> IO ()
mainFunction logPath = do
  wholeLog <- TIO.readFile logPath
  logLines <- return $ fmap U.convertLogEntryToPath $ T.lines wholeLog
  -- TIO.putStrLn $ T.unlines $ fmap T.unwords logLines
  print ""
  print $ U.buildLogTreeNode T.empty logLines
