module LogTree.Data where

import qualified Data.Text as T

data LogTree = LogTree {
  logTreeValue :: T.Text,
  logTreeChildren :: [LogTree]
  } deriving (Eq)

instance Show LogTree where
  -- show LogTree {logTreeValue = value, logTreeChildren = children} =
  --   T.unpack value ++ "\n" ++ (concat $ fmap (\ c -> "    " ++ show c) children)
  showsPrec d LogTree {logTreeValue = value, logTreeChildren = children} =
    showString ((concat (replicate d "    ")) ++ T.unpack value ++ "\n") .
    (foldl (flip (.)) id (fmap (showsPrec (d + 1)) children))
