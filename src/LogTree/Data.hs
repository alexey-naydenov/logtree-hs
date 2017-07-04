module LogTree.Data where

import qualified Data.Text as T

data LogTree = LogTree [T.Text] deriving (Show, Eq)
