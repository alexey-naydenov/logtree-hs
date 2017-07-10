module LogTree.Data where

import qualified Data.Text as T

data LogTree = LogTree {
  logTreeValue :: T.Text,
  logTreeChildren :: [LogTree]
  } deriving (Eq, Show)


