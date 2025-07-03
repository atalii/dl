-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.
--

module Main where

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

-- A GroundFact functions like a parser's fact, but requires that every
-- variable be bound.
type GroundFact = Sentence BoundVar

data Rule = Implication Antecedent Consequent
  deriving (Eq, Show)

data SubstitutionFailure = SubstitutionFailure
  deriving (Show)

makeUniverse :: Document -> Either [String] Universe
makeUniverse (Document clauses) = case split clauses of
  ([], facts, rules) -> Right $ Universe facts rules
  (errs, _, _) -> Left errs
  where
    split :: [Clause] -> ([String], [GroundFact], [Rule])
    split = collate . map interpret

    interpret (Simple (Fact sub predicate)) = case sub of
      (Free var) -> Left $ "illegal free var: " <> var
      (Bound var) -> Right $ Left $ Fact var predicate
    -- TODO: clean this up. GroundFacts should be part of the parser, and
    -- makeUniverse shouldn't then be fallible.
    interpret
      (Rule (Fact consSub consPred) (Fact antSub antPred)) =
        Right $
          Right $
            Implication
              (Fact antSub antPred)
              (Fact consSub consPred)

    collate vals =
      let (errs, sourceClauses) = partitionEithers vals
          (facts, rules) = partitionEithers sourceClauses
       in (errs, facts, rules)

naive :: Universe -> Either [SubstitutionFailure] [GroundFact]
naive u = getFacts <$> findFixedPoint u
  where
    getFacts (Universe facts _) = facts

findFixedPoint :: Universe -> Either [SubstitutionFailure] Universe
findFixedPoint u = do
  u' <- infer u
  if u == u' then return u else findFixedPoint u'

infer :: Universe -> Either [SubstitutionFailure] Universe
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
      (Fact hSub hPred) =
        if hPred /= antPred
          then return Nothing
          else case antSub of
            Bound sub | sub == hSub -> Just <$> ground consequent []
            Bound _ -> return Nothing
            Free x -> Just <$> ground consequent [(x, hSub)]

ground :: Consequent -> [(FreeVar, BoundVar)] -> Either SubstitutionFailure GroundFact
ground (Fact (Bound var) predicate) _ = Right $ Fact var predicate
ground (Fact (Free var) predicate) subs = case [bound | (free, bound) <- subs, free == var] of
  [bound] -> Right $ Fact bound predicate
  _ -> Left SubstitutionFailure

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
    >>= handleLeft (bail 3 . concat) . makeUniverse
    >>= handleLeft (bail 4) . naive
    >>= print
  where
    handleLeft :: (a -> IO b) -> Either a b -> IO b
    handleLeft f (Left a) = f a
    handleLeft _ (Right b) = return b
