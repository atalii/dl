-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.
--
{- We use `head` for working with results from the parser, which are, by
 - the definition of the grammar, non-empty. -}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main where

import Data.Char
import Data.DL.Parser
import Data.Either
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

data Universe = Universe [GroundFact] [Rule]
  deriving (Eq, Show)

data Var = Bound BoundVar | Free FreeVar
  deriving (Eq, Show)

type BoundVar = String

type FreeVar = String

type Predicate = String

data Sentence v = Sentence v Predicate
  deriving (Eq, Show)

type GroundFact = Sentence BoundVar

type RuleAntecedent = Sentence Var

type RuleConsequent = Sentence Var

data Rule = Implication RuleAntecedent RuleConsequent
  deriving (Eq, Show)

makeUniverse :: Document -> Either [String] Universe
makeUniverse (Document clauses) = case partition clauses of
  ([], facts, rules) -> Right $ Universe facts rules
  (errs, _, _) -> Left errs
  where
    partition = collate . map interpret

    interpret (Simple (Fact sub predicate)) =
      if isUpper (head sub)
        then Left $ "illegal free var: " <> sub
        else Right $ Left $ Sentence sub predicate
    interpret
      ( Rule
          (Fact consSub consPred)
          (Antecedent (Fact antSub antPred))
        ) =
        Right $
          Right $
            Implication
              (Sentence (makeVar antSub) antPred)
              (Sentence (makeVar consSub) consPred)

    collate vals =
      let (errs, sourceClauses) = partitionEithers vals
          (facts, rules) = partitionEithers sourceClauses
       in (errs, facts, rules)

naive :: Universe -> [GroundFact]
naive u = let (Universe facts _) = findFixedPoint u in facts

findFixedPoint :: Universe -> Universe
findFixedPoint u =
  let u' = infer u
   in if u == u' then u else findFixedPoint u'

infer :: Universe -> Universe
infer (Universe facts rules) =
  let newFacts = concatMap apply rules
   in Universe (nub $ facts ++ newFacts) rules
  where
    apply :: Rule -> [GroundFact]
    apply rule = mapMaybe (apply' rule) facts

    apply' :: Rule -> GroundFact -> Maybe GroundFact
    apply'
      ( Implication
          (Sentence antSub antPred)
          consequent
        )
      (Sentence hSub hPred) =
        if hPred /= antPred
          then Nothing
          else case antSub of
            Bound sub | sub == hSub -> Just $ ground consequent []
            Bound _ -> Nothing
            Free x -> Just $ ground consequent [(x, hSub)]

ground :: RuleConsequent -> [(FreeVar, BoundVar)] -> GroundFact
ground (Sentence (Bound var) predicate) _ = Sentence var predicate
ground (Sentence (Free var) predicate) subs = case [bound | (free, bound) <- subs, free == var] of
  [bound] -> Sentence bound predicate
  _ -> error "lol"

makeVar :: String -> Var
makeVar s = if isUpper (head s) then Free s else Bound s

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
main = readDocument >>= eval . makeUniverse
  where
    eval (Left errs) = bail 3 $ concat errs -- LOL
    eval (Right universe) = print $ naive universe
