module Util (joinEithers) where

joinEithers :: [Either [a] [b]] -> Either [a] [b]
joinEithers [] = return []
joinEithers ((Right x) : xs) = (++ x) <$> joinEithers xs
joinEithers ((Left x) : xs) = Left $ x ++ gatherLefts xs
  where
    gatherLefts [] = []
    gatherLefts ((Left y) : ys) = y ++ gatherLefts ys
    gatherLefts ((Right _) : ys) = gatherLefts ys
