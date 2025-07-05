-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.

module Data.DL.Util (joinEithers) where

joinEithers :: [Either [a] [b]] -> Either [a] [b]
joinEithers [] = return []
joinEithers ((Right x) : xs) = (++ x) <$> joinEithers xs
joinEithers ((Left x) : xs) = Left $ x ++ gatherLefts xs
  where
    gatherLefts [] = []
    gatherLefts ((Left y) : ys) = y ++ gatherLefts ys
    gatherLefts ((Right _) : ys) = gatherLefts ys
