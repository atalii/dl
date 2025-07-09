-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.

module Data.DL.Evaluation.Naive (naive) where

import Control.Monad
import Data.DL.Evaluation
import Data.DL.Evaluation.Util
import Data.DL.Parser
import Data.DL.Universe
import Data.DL.Util
import Data.Either
import Data.Either.Extra
import Data.List
import Data.Maybe

-- | Run the naive evaluation strategy by repeatedly making as many inferences
-- as possible until we reach a fixed point.
naive :: Universe -> Either [EvalError] [GroundFact]
naive u = getFacts <$> findFixedPoint u

findFixedPoint :: Universe -> Either [EvalError] Universe
findFixedPoint u = do
  u' <- infer u
  if u == u' then return u else findFixedPoint u'

infer :: Universe -> Either [EvalError] Universe
infer (Universe facts rules bounds) =
  addFactsToUniverse <$> joinEithers (map (mapLeft singleton . apply) rules)
  where
    addFactsToUniverse newFacts = Universe (nub $ facts ++ newFacts) rules bounds

    apply :: Rule -> Either EvalError [GroundFact]
    apply rule =
      let ruleCandidates = bindVarsIn rule
          affirmedConsequents = mapMaybe affirm <$> ruleCandidates
       in concatMap decomposeFacts <$> affirmedConsequents

    bindVarsIn :: Rule -> Either EvalError [GroundRule]
    bindVarsIn rule =
      let frees = gatherFrees rule
          substitutionTables = map (zip frees) $ replicateM (length frees) bounds
       in mapM (bindRule rule) substitutionTables

    affirm (GroundRule (Fact antecedent) consequent) =
      if antecedent `elem` facts then Just consequent else Nothing
    affirm (GroundRule (Conjunct lhs rhs) consequent) =
      affirm (GroundRule lhs consequent) <* affirm (GroundRule rhs consequent)

    decomposeFacts :: GroundConsequent -> [GroundFact]
    decomposeFacts claim = [claim]
