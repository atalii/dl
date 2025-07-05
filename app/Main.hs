-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Monad
import Data.DL.Evaluation.Naive
import Data.DL.Parser
import Data.DL.Universe
import System.Environment
import System.Exit
import System.IO

bail :: (Show a) => Int -> a -> IO b
bail code msg = getProgName >>= hPutStrLn stderr . (<> ": " <> show msg) >> exitWith (ExitFailure code)

readDocument :: IO Document
readDocument =
  getArgs >>= getTarget >>= runDocumentParser >>= handleParseFailure
  where
    getTarget [fileName] = return fileName
    getTarget _ = bail 1 "bad usage"

    handleParseFailure (Left parseError) = bail 2 parseError
    handleParseFailure (Right doc) = return doc

main :: IO ()
main =
  readDocument
    >>= handleLeft (logErrs >=> bail 3) . naive . makeUniverse
    >>= putStrLn . unlines . map show
  where
    handleLeft :: (a -> IO b) -> Either a b -> IO b
    handleLeft f (Left a) = f a
    handleLeft _ (Right b) = return b

    logErrs :: (Show a) => [a] -> IO String
    logErrs vars = mapM_ (hPrint stderr) vars >> return "fatal error"
