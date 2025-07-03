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
import Text.Parsec.Pos
import Util

data Universe = Universe [GroundFact] [Rule]
  deriving (Eq, Show)

-- A GroundFact functions like a parser's fact, but requires that every
-- variable be bound.
type GroundFact = Sentence BoundVar

data Rule = Implication Antecedent Consequent
  deriving (Eq, Show)

data PosTagged a = Tag SourcePos a

data EvalError = SubstitutionFailure FreeVar | InvalidFree FreeVar

instance Show EvalError where
  show (SubstitutionFailure name) = "no valid substitutions for unbound variable: " <> show name <> "."
  show (InvalidFree name) = "free variable disallowed: " <> show name <> "."

instance (Show a) => Show (PosTagged a) where
  show (Tag pos a) = show pos <> ": " <> show a

makeUniverse :: Document -> Either [PosTagged EvalError] Universe
makeUniverse (Document clauses) = case split clauses of
  ([], facts, rules) -> Right $ Universe facts rules
  (errs, _, _) -> Left errs
  where
    split :: [Clause] -> ([PosTagged EvalError], [GroundFact], [Rule])
    split = collate . map interpret

    interpret (Simple (Fact sub predicate)) = case sub of
      (Free pos var) -> Left $ Tag pos $ InvalidFree var
      (Bound _ var) -> Right $ Left $ Fact var predicate
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
      (Fact hSub hPred) =
        if hPred /= antPred
          then return Nothing
          else case antSub of
            Bound _ sub | sub == hSub -> Just <$> ground consequent []
            Bound _ _ -> return Nothing
            Free _ x -> Just <$> ground consequent [(x, hSub)]

ground :: Consequent -> [(FreeVar, BoundVar)] -> Either (PosTagged EvalError) GroundFact
ground (Fact (Bound _ var) predicate) _ = Right $ Fact var predicate
ground (Fact (Free pos var) predicate) subs = case [bound | (free, bound) <- subs, free == var] of
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
    >>= handleLeft (logErrs >=> bail 3) . makeUniverse
    >>= handleLeft (logErrs >=> bail 4) . naive
    >>= print
  where
    handleLeft :: (a -> IO b) -> Either a b -> IO b
    handleLeft f (Left a) = f a
    handleLeft _ (Right b) = return b

    logErrs :: (Show a) => [a] -> IO String
    logErrs vars = mapM_ (hPrint stderr) vars >> return "fatal error"
