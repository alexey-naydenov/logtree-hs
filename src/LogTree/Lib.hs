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
  print $
    U.trimExcessChildren 20 $
    U.mergeSingleChild $
    U.buildLogTreeNode 6 T.empty logLines
