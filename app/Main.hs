-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.
--

module Main where

import Control.Monad
import Data.DL.Parser
import Data.Either
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import Util

data Universe = Universe [GroundFact] [Rule]
  deriving (Eq, Show)

data Rule = Implication Antecedent Consequent
  deriving (Eq, Show)

newtype EvalError = SubstitutionFailure FreeVar

instance Show EvalError where
  show (SubstitutionFailure (Tag _ name)) = "no valid substitutions for unbound variable: " <> show name <> "."

makeUniverse :: Document -> Universe
makeUniverse (Document clauses) =
  let (facts, rules) = split
   in Universe facts rules
  where
    split :: ([GroundFact], [Rule])
    split = partitionEithers $ map interpret clauses

    interpret :: Clause -> Either GroundFact Rule
    interpret (Simple fact) = Left fact
    interpret
      (Rule (Fact consSub consPred) (Fact antSub antPred)) =
        Right $
          Implication
            (Fact antSub antPred)
            (Fact consSub consPred)

naive :: Universe -> Either [PosTagged EvalError] [GroundFact]
naive u = getFacts <$> findFixedPoint u
  where
    getFacts (Universe facts _) = facts

findFixedPoint :: Universe -> Either [PosTagged EvalError] Universe
findFixedPoint u = do
  u' <- infer u
  if u == u' then return u else findFixedPoint u'

infer :: Universe -> Either [PosTagged EvalError] Universe
infer (Universe facts rules) = addFactsToUniverse <$> joinEithers (map apply rules)
  where
    addFactsToUniverse newFacts = Universe (nub $ facts ++ newFacts) rules

    apply rule = case partitionEithers $ map (apply' rule) facts of
      ([], mgf) -> Right $ catMaybes mgf
      (failures, _) -> Left failures

    apply'
      ( Implication
          (Fact antSub antPred)
          consequent
        )
      (Fact hSub@(Tag _ hName) hPred) =
        if hPred /= antPred
          then return Nothing
          else case antSub of
            Bound (Tag _ sub) | sub == hName -> Just <$> ground consequent []
            Bound _ -> return Nothing
            Free x -> Just <$> ground consequent [(x, hSub)]

ground :: Consequent -> [(FreeVar, BoundVar)] -> Either (PosTagged EvalError) GroundFact
ground (Fact (Bound var) predicate) _ = Right $ Fact var predicate
ground (Fact (Free var@(Tag pos _)) predicate) subs = case [bound | (free, bound) <- subs, free == var] of
  [bound] -> Right $ Fact bound predicate
  _ -> Left $ Tag pos $ SubstitutionFailure var

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
