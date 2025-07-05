-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.

module Data.DL.Evaluation.Naive (naive) where

import Data.DL.Evaluation
import Data.DL.Parser
import Data.DL.Universe
import Data.DL.Util
import Data.Either
import Data.List
import Data.Maybe

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
